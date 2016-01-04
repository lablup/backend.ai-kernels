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
	"path"
	"path/filepath"
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
var forkExecExceptions map[string]struct{} = nil
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

func traceProcess(pid int) {
	l := log.New(os.Stderr, "", 0)
	for {
		var state syscall.WaitStatus
		traceePid, _ := syscall.Wait4(-1, &state, syscall.WALL, nil)
		if state.Exited() {
			if debug {
				l.Printf("EXIT (pid %d) status %d\n", traceePid, state.ExitStatus())
			}
			if pid == traceePid {
				break
			}
		} else if state.Stopped() {
			if debug {
				l.Printf("STOP stopsignal: 0x%x, trapcause: %d\n",
					uint(state.StopSignal()), state.TrapCause())
			}
			switch state.StopSignal() {
			case syscall.SIGSTOP:
				syscall.PtraceCont(int(traceePid), 0)
			case syscall.SIGTRAP:
				allow := true
				var regs syscall.PtraceRegs
				syscall.PtraceGetRegs(traceePid, &regs)
				syscallId := uint(regs.Orig_rax)
				switch state.TrapCause() {
				case 7 /*PTRACE_EVENT_SECCOMP*/ :
					switch seccomp.ScmpSyscall(syscallId) {
					case id_Fork, id_Vfork, id_Clone:
						execPath, _ := getExecutable(traceePid)
						if execPath == myExecPath {
							allow = true
						} else if execPath == intraJailPath {
							allow = true
						} else if _, ok := forkExecExceptions[execPath]; ok {
							allow = true
						} else {
							allow = (forkCount < policyInst.GetForkAllowance())
							forkCount++
						}
						if debug {
							l.Printf("fork owner: %s\n", execPath)
						}
					case id_Execve:
						execPath, _ := getExecutable(traceePid)
						if execPath == myExecPath {
							allow = true
						} else if execPath == intraJailPath {
							allow = true
						} else if _, ok := forkExecExceptions[execPath]; ok {
							allow = true
						} else {
							allow = (execCount < policyInst.GetForkAllowance())
							execCount++
						}
						if debug {
							l.Printf("execve owner: %s\n", execPath)
						}
					case id_Open:
						// TODO: extract path from syscall args
						allow = policyInst.CheckPath("...", policy.PERM_RD)
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
						syscall.PtraceSetRegs(traceePid, &regs)
					}
				case syscall.PTRACE_EVENT_CLONE,
					syscall.PTRACE_EVENT_FORK,
					syscall.PTRACE_EVENT_VFORK:
					// nothing to do for our case.
					if debug {
						childPid, _ := syscall.PtraceGetEventMsg(traceePid)
						l.Printf("FORK/CLONE (%d spawned child %d)\n", traceePid, childPid)
					}
				}
				syscall.PtraceCont(traceePid, 0)
			case syscall.SIGCHLD:
				// nothing to do for our case.
				if debug {
					var siSize = C.getSizeOfSiginfo()
					var siBuf = make([]byte, siSize)
					var siBufPtr = unsafe.Pointer(&siBuf)
					syscall.RawSyscall6(syscall.SYS_PTRACE, syscall.PTRACE_GETSIGINFO,
						uintptr(traceePid), 0, uintptr(siBufPtr), 0, 0)
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
				syscall.PtraceCont(traceePid, 0)
			}
		}
	}
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
	args := append([]string{intraJailPath}, os.Args[2:]...)
	forkExecExceptions = make(map[string]struct{})
	for _, path := range policyInst.GetForkExecExceptionPaths() {
		forkExecExceptions[path] = struct{}{}
	}
	cwd, _ := os.Getwd()
	envs := filterEnvs(os.Environ(), policyInst.GetPreservedEnvKeys())
	envs = append(envs, policyInst.GetExtraEnvs()...)
	pid, err := syscall.ForkExec(args[0], args, &syscall.ProcAttr{
		cwd,
		envs,
		[]uintptr{0, 1, 2},
		&syscall.SysProcAttr{Ptrace: true},
	})
	if err != nil {
		l.Fatal("ForkExec: ", err)
	}
	var ptraceOpts int = 0
	ptraceOpts = (1 << 7 /*PTRACE_O_TRACESECCOMP*/)
	ptraceOpts |= syscall.PTRACE_O_TRACECLONE
	ptraceOpts |= syscall.PTRACE_O_TRACEFORK
	ptraceOpts |= syscall.PTRACE_O_TRACEVFORK
	err = syscall.PtraceSetOptions(pid, ptraceOpts)
	if err != nil {
		l.Fatal("PtraceSetOptions: ", err)
	}
	traceProcess(pid)
}

// vim: ts=4 sts=4 sw=4 noet
