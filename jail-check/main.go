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

var (
	myExecPath, _ = utils.GetExecutable(os.Getpid())
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

func setPtraceOpts(l *log.Logger, pid int) {
	var ptraceOpts int = (1 << 7 /* SECCOMP */)
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
	traceLogFile, err := os.Create("/tmp/trace.log")
	if err != nil {
		l.Fatal("Could not create /tmp/trace.log file.: ", err)
	}
	traceLog := log.New(traceLogFile, "", 0)
	defer traceLogFile.Close()

	allowedSyscalls := make(map[string]struct{})
	for _, name := range policy.AllowedSyscalls {
		allowedSyscalls[name] = struct{}{}
	}
	for _, name := range policy.TracedSyscalls {
		allowedSyscalls[name] = struct{}{}
	}

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
				color.Set(color.FgYellow)
				l.Printf("EXIT (pid %d) status %d\n", result.pid, result.status.ExitStatus())
				color.Unset()
				if pid == result.pid {
					// Our very child has exited. Terminate.
					break loop
				}
			} else if result.status.Stopped() {
				switch result.status.StopSignal() {
				case syscall.SIGSTOP:
					if first_stop {
						setPtraceOpts(l, pid)
						first_stop = false
					}
				case syscall.SIGTRAP:
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
					switch result.status.TrapCause() {
					case 7 /*PTRACE_EVENT_SECCOMP*/ :
						switch seccomp.ScmpSyscall(syscallId) {
						case id_Fork, id_Vfork, id_Clone:
							execPath, _ := utils.GetExecutable(result.pid)
							forkCount++
							traceLog.Printf("fork/vfork/clone() (count: %d) on %s\n", forkCount, execPath)
						case id_Execve:
							execPath, _ := utils.GetExecutable(result.pid)
							execCount++
							traceLog.Printf("execve() (count: %d) on %s\n", execCount, execPath)
						default:
							syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
							_, ok := allowedSyscalls[syscallName]
							if !ok {
								color.Set(color.FgRed)
								l.Printf("non-registered syscall: %s\n", syscallName)
								color.Unset()
								traceLog.Printf("non-registered syscall: %s\n", syscallName)
							}
						}
					case 0:
						// ignore
					default:
						l.Printf("Unknown trap cause: %d\n", result.status.TrapCause())
					}
				default:
					// Transparently deliver other signals.
					color.Set(color.FgBlue)
					l.Printf("Injecting unhandled signal: %v\n", result.status.StopSignal())
					color.Unset()
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
