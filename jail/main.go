// This ptrace-based jail assumes linux/amd64 platforms.

/*
Command-line usage:
	./jail <policy_name> <child_args ...>

Example:
	./jail python3 /bin/sh /home/sorna/run.sh
*/
package main

import (
	"fmt"
	seccomp "github.com/seccomp/libseccomp-golang"
	"log"
	"os"
	"os/signal"
	"path"
	"path/filepath"
	"runtime"
	"sorna-repl/jail/policy"
	"strings"
	"syscall"
	"unsafe"
)

/*
#include <signal.h>
#include <sys/types.h>

size_t getSizeOfSiginfo() { return sizeof(siginfo_t); }
int getSignalFromSI(void *t) { return ((siginfo_t *) t)->si_signo; }
int getCodeFromSI(void *t) { return ((siginfo_t *) t)->si_code; }
int getStatusFromSI(void *t) { return ((siginfo_t *) t)->si_status; }
*/
import "C"

const debug = false

var (
	myExecPath, _ = getExecutable(os.Getpid())
	myPath        = filepath.Dir(myExecPath)
	intraJailPath = path.Join(myPath, "intra-jail")
	arch, _       = seccomp.GetNativeArch()
	id_Open, _    = seccomp.GetSyscallFromNameByArch("open", arch)
	id_Clone, _   = seccomp.GetSyscallFromNameByArch("clone", arch)
	id_Fork, _    = seccomp.GetSyscallFromNameByArch("fork", arch)
	id_Vfork, _   = seccomp.GetSyscallFromNameByArch("vfork", arch)
	id_Execve, _  = seccomp.GetSyscallFromNameByArch("execve", arch)
)
var policyInst policy.SandboxPolicy = nil
var execCount uint = 0
var forkCount uint = 0

func filterEnvs(envs []string, preservedKeys []string) []string {
	filteredEnvs := []string{}
	for _, entry := range os.Environ() {
		var kept bool = false
		for _, key := range preservedKeys {
			if strings.HasPrefix(entry, key+"=") {
				kept = true
				break
			}
		}
		if kept {
			filteredEnvs = append(filteredEnvs, entry)
		}
	}
	return filteredEnvs
}

func setPtraceOpts(l *log.Logger, pid int) {
	var ptraceOpts int = 0
	// EXITKILL option ensures to kill all children/tracee processes
	// when the parent (me) exits.
	ptraceOpts = (1 << 7 /*PTRACE_O_TRACESECCOMP*/)
	ptraceOpts |= (1 << 20 /*PTRACE_O_EXITKILL, Linux >= 3.4 */)
	//ptraceOpts |= syscall.PTRACE_O_TRACECLONE
	//ptraceOpts |= syscall.PTRACE_O_TRACEFORK
	//ptraceOpts |= syscall.PTRACE_O_TRACEVFORK
	if err := syscall.PtraceSetOptions(pid, ptraceOpts); err != nil {
		l.Fatal("PtraceSetOptions: ", err)
	}
}

type WaitResult struct {
	pid int
	err error
	status syscall.WaitStatus
}

func traceProcess(l *log.Logger, pid int) {
	first_stop := true
	mySignals := make(chan os.Signal, 1)
	childrenWaits := make(chan WaitResult, 2)
	signal.Notify(mySignals, os.Interrupt, syscall.SIGTERM)
loop:
	for {
		go func() {
			var status syscall.WaitStatus
			traceePid, err := syscall.Wait4(-1, &status, syscall.WALL, nil)
			childrenWaits <- WaitResult{int(traceePid), err, status}
		}()
		select {
		case sig := <-mySignals:
			switch sig {
			case os.Interrupt, syscall.SIGTERM:
				// Terminate all my children.
				// Since we set Setsid: true in SysProcAttr of syscall.ForkExec(),
				// the signals we receive are NOT automatically delivered to children.
				// We control the SIGINT/SIGTERM behvaiour gracefully for later
				// extension of Sorna.
				sid, _, _ := syscall.RawSyscall(syscall.SYS_GETSID, uintptr(pid), 0, 0)
				syscall.Kill(int(sid), syscall.SIGINT)
			}
		case result := <-childrenWaits:
			var signalToChild syscall.Signal = 0
			if result.err != nil {
				switch result.err.(syscall.Errno) {
				case syscall.EINTR:
					// Retry the wait system call.
					continue loop
				case syscall.ECHILD:
					// No child processes found. Terminate.
					break loop
				}
			}
			if result.status.Exited() {
				if debug {
					l.Printf("EXIT (pid %d) status %d\n", result.pid, result.status.ExitStatus())
				}
				if pid == result.pid {
					// Our very child has exited. Terminate.
					break loop
				}
			} else if result.status.Stopped() {
				if debug {
					l.Printf("STOP stopsignal: 0x%x, trapcause: %d\n",
						uint(result.status.StopSignal()), result.status.TrapCause())
				}
				switch result.status.StopSignal() {
				case syscall.SIGSTOP:
					if first_stop {
						setPtraceOpts(l, pid)
						first_stop = false
					}
				case syscall.SIGTRAP:
					allow := true
					var regs syscall.PtraceRegs
					for {
						err := syscall.PtraceGetRegs(result.pid, &regs)
						if (err != nil) {
							errno := err.(syscall.Errno)
							if errno == syscall.EBUSY || errno == syscall.EFAULT || errno == syscall.ESRCH {
								continue
							}
						}
						break
					}
					syscallId := uint(regs.Orig_rax)
					switch result.status.TrapCause() {
					case 7 /*PTRACE_EVENT_SECCOMP*/ :
						switch seccomp.ScmpSyscall(syscallId) {
						case id_Fork, id_Vfork, id_Clone:
							execPath, _ := getExecutable(result.pid)
							if execPath == myExecPath {
								allow = true
							} else if execPath == intraJailPath {
								allow = true
							} else {
								allow = (forkCount < policyInst.GetForkAllowance())
								forkCount++
							}
							if debug {
								l.Printf("fork owner: %s\n", execPath)
							}
						case id_Execve:
							execPath, _ := getExecutable(result.pid)
							if execPath == myExecPath {
								allow = true
							} else if execPath == intraJailPath {
								allow = true
							} else if policyInst.CheckPathExecutable(execPath) {
								allow = true
							} else {
								allow = (execCount < policyInst.GetExecAllowance())
								execCount++
							}
							if debug {
								l.Printf("execve owner: %s\n", execPath)
							}
						case id_Open:
							// TODO: extract path from syscall args
							allow = policyInst.CheckPathAccessible("...", policy.PERM_RD)
						default:
							allow = true
						}
						if !allow {
							if debug {
								syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
								l.Printf("skipped syscall %s\n", syscallName)
							}
							// Skip the system call with permission error
							regs.Orig_rax = 0xFFFFFFFFFFFFFFFF // -1
							regs.Rax = 0xFFFFFFFFFFFFFFFF - uint64(syscall.EPERM) + 1
							syscall.PtraceSetRegs(result.pid, &regs)
						} else {
							if debug {
								syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
								l.Printf("allowed syscall %s\n", syscallName)
							}
						}
					case syscall.PTRACE_EVENT_CLONE,
						syscall.PTRACE_EVENT_FORK,
						syscall.PTRACE_EVENT_VFORK:
						// nothing to do for our case.
						if debug {
							childPid, _ := syscall.PtraceGetEventMsg(result.pid)
							l.Printf("FORK/CLONE (%d spawned child %d)\n", result.pid, childPid)
						}
					case 0:
						// ignore
					default:
						l.Printf("Unknown trap cause: %d\n", result.status.TrapCause())
					}
				case syscall.SIGCHLD:
					// nothing to do for our case.
					if debug {
						var siSize = C.getSizeOfSiginfo()
						var siBuf = make([]byte, siSize)
						var siBufPtr = unsafe.Pointer(&siBuf)
						syscall.RawSyscall6(syscall.SYS_PTRACE, syscall.PTRACE_GETSIGINFO,
							uintptr(result.pid), 0, uintptr(siBufPtr), 0, 0)
						var childSignal = C.getSignalFromSI(siBufPtr)
						var childCode = C.getCodeFromSI(siBufPtr)
						var childStatus = C.getStatusFromSI(siBufPtr)
						if int(childSignal) == int(syscall.SIGCHLD) && childCode == 1 /*CLD_EXITED*/ {
							// pass
						} else {
							l.Printf("SIGCHLD: signal=0x%x, code=0x%x, status=%d\n",
								childSignal, childCode, childStatus)
						}
					}
					signalToChild = result.status.StopSignal()
				default:
					// Transparently deliver other signals.
					if debug {
						l.Printf("Injecting unhandled signal: %v\n", result.status.StopSignal())
					}
					signalToChild = result.status.StopSignal()
				}
				for {
					// TODO: refactor using function-accepting function
					err := syscall.PtraceCont(result.pid, int(signalToChild))
					if (err != nil) {
						errno := err.(syscall.Errno)
						if errno == syscall.EBUSY || errno == syscall.EFAULT || errno == syscall.ESRCH {
							continue
						}
					}
					break
				}
			}
		} /* endselect */
	} /* endloop */
}

func getExecutable(pid int) (string, error) {
	const deletedTag = " (deleted)"
	execPath, err := os.Readlink(fmt.Sprintf("/proc/%d/exe", pid))
	if err != nil {
		return execPath, err
	}
	execPath = strings.TrimSuffix(execPath, deletedTag)
	execPath = strings.TrimPrefix(execPath, deletedTag)
	switch execPath {
	case "/bin/sh", "/bin/bash", "/bin/dash":
		rawData := make([]byte, 1024)
		file, err := os.Open(fmt.Sprintf("/proc/%d/cmdline", pid))
		if err != nil {
			return execPath, err
		}
		file.Read(rawData)
		file.Close()
		data := string(rawData[:])
		cmd := strings.Split(data, "\x00")
		if !path.IsAbs(cmd[1]) && cmd[1] != "/usr/bin/env" {
			cwd, err := os.Readlink(fmt.Sprintf("/proc/%d/cwd", pid))
			if err != nil {
				return execPath, err
			}
			execPath = path.Join(cwd, cmd[1])
		} else {
			execPath = cmd[1]
		}
	}
	return execPath, nil
}

func main() {
	l := log.New(os.Stderr, "", 0)
	if len(os.Args) < 2 {
		l.Fatal("Main: Not enough command-line arguments. See the docs.")
	}
	policyName := os.Args[1]
	var err error
	policyInst, err = policy.GeneratePolicy(policyName)
	if err != nil {
		l.Fatal("GeneratePolicy: ", err)
	}

	/* Initialize fork/exec of the child. */
	runtime.GOMAXPROCS(1)
	runtime.LockOSThread()
	// Locking the OS thread is required to let syscall.Wait4() work correctly
	// because waitpid() only monitors the caller's direct children, not
	// siblings' children.
	args := append([]string{intraJailPath}, os.Args[2:]...)
	cwd, _ := os.Getwd()
	envs := filterEnvs(os.Environ(), policyInst.GetPreservedEnvKeys())
	envs = append(envs, policyInst.GetExtraEnvs()...)
	pid, err := syscall.ForkExec(args[0], args, &syscall.ProcAttr{
		cwd,
		envs,
		[]uintptr{0, 1, 2},
		&syscall.SysProcAttr{Setsid: true, Ptrace: true},
	})
	if err != nil {
		l.Fatal("ForkExec: ", err)
	}
	traceProcess(l, pid)
}

// vim: ts=4 sts=4 sw=4 noet
