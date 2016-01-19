package main

import (
	"fmt"
	seccomp "github.com/seccomp/libseccomp-golang"
	"log"
	"os"
	"os/signal"
	"runtime"
	"sorna-repl/jail/policy"
	"syscall"
)

var (
	myExecPath, _ = getExecutable(os.Getpid())
	myPath        = filepath.Dir(myExecPath)
	intraJailPath = path.Join(myPath, "intra-jail-check")
	arch, _       = seccomp.GetNativeArch()
)
var execCount uint = 0
var forkCount uint = 0

func setPtraceOpts(l *log.Logger, pid int) {
	var ptraceOpts int = syscall.PTRACE_O_SYSGOOD
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
	traceLogFile, err := os.Create("/tmp/trace.log")
	if err {
		l.Fatal("Could not create /tmp/trace.log file.: ", err)
	}
	traceLog := log.New(traceLogFile, "", 0)
	defer traceLogFile.Close()
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
							forkCount++
							traceLog.Printf("fork/vfork/clone() (count: %d) on %s\n", forkCount, execPath)
							if debug {
								l.Printf("fork owner: %s\n", execPath)
							}
						case id_Execve:
							execPath, _ := getExecutable(result.pid)
							execCount++
							traceLog.Printf("execve() (count: %d) on %s\n", execCount, execPath)
							if debug {
								l.Printf("execve owner: %s\n", execPath)
							}
						case id_Open:
							// TODO: extract path from syscall args
						default:
							syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
							traceLog.Printf("non-registered syscall: %s\n", syscallName)
						}
					case 0:
						// ignore
					default:
						l.Printf("Unknown trap cause: %d\n", result.status.TrapCause())
					}
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

func main() {
	l := log.New(os.Stderr, "", 0)
	if len(os.Args) < 2 {
		l.Fatal("Main: Not enough command-line arguments. See the docs.")
	}
	/* Initialize fork/exec of the child. */
	runtime.GOMAXPROCS(1)
	runtime.LockOSThread()
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
