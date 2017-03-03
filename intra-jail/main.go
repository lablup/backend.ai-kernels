package main

import (
	"github.com/fatih/color"
	seccomp "github.com/seccomp/libseccomp-golang"
	"log"
	"os"
	"sorna-repl/jail/policy"
	"syscall"
)

func main() {

	l := log.New(os.Stderr, "", 0)

	syscall.RawSyscall(syscall.SYS_PRCTL, syscall.PR_SET_PTRACER, uintptr(os.Getppid()), 0)

	arch, _ := seccomp.GetNativeArch()
	laterFilter, _ := seccomp.NewFilter(seccomp.ActErrno.SetReturnCode(int16(syscall.EPERM)))
	for _, syscallName := range policy.AllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		laterFilter.AddRuleExact(syscallId, seccomp.ActAllow)
	}
	for _, syscallName := range policy.TracedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		laterFilter.AddRuleExact(syscallId, seccomp.ActTrace)
	}
	killSyscalls := []string{"kill", "killpg", "tkill", "tgkill"}
	for _, syscallName := range killSyscalls {
		scId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		laterFilter.AddRuleExact(scId, seccomp.ActTrace)
	}
	for syscallName, cond := range policy.ConditionallyAllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		laterFilter.AddRuleConditional(syscallId, seccomp.ActAllow, []seccomp.ScmpCondition{cond})
	}
	laterFilter.SetNoNewPrivsBit(true)

	// Inform the parent that I'm ready to continue.
	// Any code before this line code must use only non-traced system calls in
	// the filter because the tracer has not set up itself yet.
	// (traced syscalls will cause ENOSYS "function not implemented" error)
	syscall.Kill(os.Getpid(), syscall.SIGSTOP)

	// Now we have the working tracer parent.
	// Make kill() syscall to be traced as well for more sophisticated filtering.
	err := laterFilter.Load()
	if err != nil {
		color.Set(color.FgRed)
		l.Printf("ScmpFilter.Load (2): ", err)
		color.Unset()
		os.Exit(1)
	}
	laterFilter.Release()

	// NOTE: signal.Reset() here causes race conditions with the tracer.
	// (syscall tracing doesn't work deterministically with it.)

	// Replace myself with the language runtime.
	err = syscall.Exec(os.Args[1], os.Args[1:], os.Environ())

	// NOTE: "function not implemented" errors here may be due to above codes.
	color.Set(color.FgRed)
	l.Printf("Exec: %s\n", err)
	color.Unset()
	os.Exit(1)
}

// vim: ts=4 sts=4 sw=4 noet
