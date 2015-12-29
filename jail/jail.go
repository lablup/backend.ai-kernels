// This ptrace-based jail assumes linux/amd64 platforms.

/*
Command-line usage:
	./jail <policy_name> <child_args ...>

Example:
	./jail python3 /bin/sh /home/sorna/run.sh
*/
package main

import (
	"github.com/kardianos/osext"
	seccomp "github.com/seccomp/libseccomp-golang"
	"log"
	"os"
	"path"
	"sorna-repl/jail/policy"
	"syscall"
)

func main() {
	var allowedSyscallsSet = make(map[string]int)
	for _, name := range policy.AllowedSyscalls {
		allowedSyscallsSet[name] = 1
	}
	arch, _ := seccomp.GetNativeArch()
	var (
		id_Open, _   = seccomp.GetSyscallFromNameByArch("open", arch)
		id_Execve, _ = seccomp.GetSyscallFromNameByArch("execve", arch)
	)
	l := log.New(os.Stderr, "", 0)
	if len(os.Args) < 2 {
		l.Print("You need to specify jail ID, absolute path to executable and its arguments.")
		l.Fatal("Error: Not enough command-line arguments.")
	}
	policyName := os.Args[1]
	myExecPath, _ := osext.Executable()
	myPath, _ := path.Split(myExecPath)
	args := append([]string{path.Join(myPath, "intra-jail")}, os.Args[2:]...)
	policyInst, err := policy.GeneratePolicy(policyName)
	if err != nil {
		l.Fatal("GeneratePolicy: ", err)
	}
	cwd, _ := os.Getwd()
	envs := append([]string{}, policyInst.GetExtraEnvs()...)
	pid, err := syscall.ForkExec(args[0], args, &syscall.ProcAttr{
		cwd,
		envs,
		[]uintptr{0, 1, 2},
		&syscall.SysProcAttr{Ptrace: true},
	})
	if err != nil {
		l.Fatal("ForkExec: ", err)
	}
	syscall.PtraceSetOptions(pid, 1<<7 /*PTRACE_O_TRACESECCOMP*/)
	var execveCount uint = 0
	for {
		syscall.PtraceCont(pid, 0)
		var state syscall.WaitStatus
		syscall.Wait4(pid, &state, 0, nil)
		if state.Exited() {
			l.Print("child-exit-status: ", state.ExitStatus(), state.TrapCause())
			break
		} else if state.Stopped() && (state.StopSignal()&7 /*PTRACE_EVENT_SECCOMP*/ != 0) {
			var regs syscall.PtraceRegs
			syscall.PtraceGetRegs(pid, &regs)
			syscallId := uint(regs.Orig_rax)
			allow := false
			switch seccomp.ScmpSyscall(syscallId) {
			case id_Open:
				// TODO: extract path from syscall args
				allow = policyInst.CheckPath("...", policy.PERM_RD)
			case id_Execve:
				// Allow only once!
				allow = (execveCount < policyInst.GetExecAllowance())
				execveCount++
			default:
				allow = true
			}
			syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
			if allow {
				l.Printf("traced syscall %s\n", syscallName)
			} else {
				// Skip the system call with permission error
				regs.Orig_rax = 0xFFFFFFFFFFFFFFFF // -1
				regs.Rax = 0xFFFFFFFFFFFFFFFF - uint64(syscall.EPERM) + 1
				syscall.PtraceSetRegs(pid, &regs)
			}
		} else {
			//l.Print("child-update: ", state.ExitStatus(), state.TrapCause())
		}
	}
}

// vim: ts=4 sts=4 sw=4 noet
