// This ptrace-based jail assumes linux/amd64 platforms.

/*
Command-line usage:
	./jail <policy_name> <child_args ...>

Example:
	./jail python3 /bin/sh /home/sorna/run.sh
*/
package main

import (
	"github.com/fatih/color"
	seccomp "github.com/seccomp/libseccomp-golang"
	"log"
	"os"
	"os/signal"
	"path"
	"path/filepath"
	"runtime"
	"sorna-repl/jail/policy"
	"sorna-repl/jail/utils"
	"syscall"
)

/*
#include <signal.h>
#include <sys/types.h>
// cgo requires an explicit export of anonymous typedefs.
typedef siginfo_t Siginfo_t;
*/
import "C"

const debug = false

var (
	myExecPath, _  = utils.GetExecutable(os.Getpid())
	myPath         = filepath.Dir(myExecPath)
	intraJailPath  = path.Join(myPath, "intra-jail")
	arch, _        = seccomp.GetNativeArch()
	id_Open, _     = seccomp.GetSyscallFromNameByArch("open", arch)
	id_Access, _   = seccomp.GetSyscallFromNameByArch("access", arch)
	id_Clone, _    = seccomp.GetSyscallFromNameByArch("clone", arch)
	id_Fork, _     = seccomp.GetSyscallFromNameByArch("fork", arch)
	id_Vfork, _    = seccomp.GetSyscallFromNameByArch("vfork", arch)
	id_Execve, _   = seccomp.GetSyscallFromNameByArch("execve", arch)
	id_Kill, _     = seccomp.GetSyscallFromNameByArch("kill", arch)
	id_Chmod, _    = seccomp.GetSyscallFromNameByArch("chmod", arch)
	id_Fchmodat, _ = seccomp.GetSyscallFromNameByArch("fchmodat", arch)
)
var policyInst policy.SandboxPolicy = nil
var execCount int = 0
var forkCount int = 0
var childCount uint = 1

func setPtraceOpts(l *log.Logger, pid int) {
	var ptraceOpts int = 0
	// EXITKILL option ensures to kill all children/tracee processes
	// when the parent (me) exits.
	ptraceOpts = (1 << 7 /*PTRACE_O_TRACESECCOMP*/)
	ptraceOpts |= (1 << 20 /*PTRACE_O_EXITKILL, Linux >= 3.4 */)
	ptraceOpts |= syscall.PTRACE_O_TRACECLONE
	ptraceOpts |= syscall.PTRACE_O_TRACEFORK
	ptraceOpts |= syscall.PTRACE_O_TRACEVFORK
	if err := syscall.PtraceSetOptions(pid, ptraceOpts); err != nil {
		l.Fatal("PtraceSetOptions: ", err)
	}
}

type WaitResult struct {
	pid    int
	err    error
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
					color.Set(color.FgYellow)
					l.Printf("EXIT (pid %d) status %d\n", result.pid, result.status.ExitStatus())
					color.Unset()
				}
				if pid == result.pid {
					// Our very child has exited. Terminate.
					break loop
				} else {
					// If we attach grand-children processes, this may be the case.
					childCount--
					if debug {
						color.Set(color.FgBlue)
						l.Printf("childCount decremented from exited subproc, now %d\n", childCount)
						color.Unset()
					}
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
						if err != nil {
							errno := err.(syscall.Errno)
							if errno == syscall.EBUSY || errno == syscall.EFAULT || errno == syscall.ESRCH {
								continue
							}
						}
						break
					}
					syscallId := uint(regs.Orig_rax)
					// Linux syscall convention for x86_64 arch:
					//  - rax: syscall number
					//  - rdi: 1st param
					//  - rsi: 2nd param
					//  - rdx: 3rd param
					//  - r10: 4th param
					//  - r8: 5th param
					//  - r9: 6th param
					switch result.status.TrapCause() {
					case 7 /*PTRACE_EVENT_SECCOMP*/ :
						switch seccomp.ScmpSyscall(syscallId) {
						case id_Fork, id_Vfork, id_Clone:
							execPath, _ := utils.GetExecutable(result.pid)
							if execPath == myExecPath {
								allow = true
							} else if execPath == intraJailPath {
								allow = true
							} else {
								maxForks := policyInst.GetForkAllowance()
								allow = (maxForks == -1 || forkCount < maxForks)
								forkCount++
							}
							allow = allow && (childCount < policyInst.GetMaxChildProcs())
							if debug {
								l.Printf("fork owner: %s\n", execPath)
							}
						case id_Kill:
							targetPid := int(regs.Rdi)
							allow = (targetPid != pid && targetPid != os.Getpid() && targetPid != 1)
						case id_Execve:
							execPath, _ := utils.GetExecutable(result.pid)
							if execPath == myExecPath {
								allow = true
							} else if execPath == intraJailPath {
								allow = true
							} else if policyInst.CheckPathExecutable(execPath) {
								allow = true
							} else {
								maxExec := policyInst.GetExecAllowance()
								allow = (maxExec == -1 || execCount < maxExec)
								execCount++
							}
							if debug {
								l.Printf("execve owner: %s\n", execPath)
							}
						case id_Open:
							pathPtr := uintptr(regs.Rdi)
							path := utils.ReadString(result.pid, pathPtr)
							// rsi is flags
							mode := int(regs.Rdx)
							allow = policyInst.CheckPathOp(path, policy.OP_OPEN, mode)
						case id_Access:
							pathPtr := uintptr(regs.Rdi)
							path := utils.ReadString(result.pid, pathPtr)
							mode := int(regs.Rsi)
							allow = policyInst.CheckPathOp(path, policy.OP_ACCESS, mode)
						case id_Fchmodat:
							pathPtr := uintptr(regs.Rsi)
							path := utils.ReadString(result.pid, pathPtr)
							path = utils.GetAbsPathAs(path, result.pid)
							mode := int(regs.Rdx)
							allow = policyInst.CheckPathOp(path, policy.OP_CHMOD, mode)
						case id_Chmod:
							pathPtr := uintptr(regs.Rdi)
							path := utils.ReadString(result.pid, pathPtr)
							path = utils.GetAbsPathAs(path, result.pid)
							mode := int(regs.Rsi)
							allow = policyInst.CheckPathOp(path, policy.OP_CHMOD, mode)
						default:
							allow = true
						}
						if !allow {
							if debug {
								syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
								color.Set(color.FgRed)
								l.Printf("blocked syscall %s\n", syscallName)
								color.Unset()
							}
							// Skip the system call with permission error
							regs.Orig_rax = 0xFFFFFFFFFFFFFFFF // -1
							regs.Rax = 0xFFFFFFFFFFFFFFFF - uint64(syscall.EPERM) + 1
							syscall.PtraceSetRegs(result.pid, &regs)
						} else {
							if debug {
								syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
								color.Set(color.FgGreen)
								l.Printf("allowed syscall %s\n", syscallName)
								color.Unset()
							}
						}
					case syscall.PTRACE_EVENT_CLONE,
						syscall.PTRACE_EVENT_FORK,
						syscall.PTRACE_EVENT_VFORK:
						childPid, _ := syscall.PtraceGetEventMsg(result.pid)
						syscall.PtraceAttach(int(childPid))
						childCount++
						if debug {
							color.Set(color.FgBlue)
							l.Printf("childCount incremented, now %d\n", childCount)
							color.Unset()
						}
					case 0:
						// ignore
					default:
						l.Printf("Unknown trap cause: %d\n", result.status.TrapCause())
					}
				case syscall.SIGCHLD:
					// SIGCHLD is not a reliable method to determine grand-children exits,
					// because multiple signals generated in a short period time may be merged
					// into a single one.
					// Instead, we use TRACE_FORK ptrace options and attaching grand-children
					// processes manually.
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
					if err != nil {
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
	envs := utils.FilterEnvs(os.Environ(), policyInst.GetPreservedEnvKeys())
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
