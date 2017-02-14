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
	filter, _ := seccomp.NewFilter(seccomp.ActErrno.SetReturnCode(int16(syscall.EPERM)))
	for _, syscallName := range policy.AllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(syscallId, seccomp.ActAllow)
	}
	for _, syscallName := range policy.TracedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(syscallId, seccomp.ActTrace)
	}
	for syscallName, cond := range policy.ConditionallyAllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleConditional(syscallId, seccomp.ActAllow, []seccomp.ScmpCondition{cond})
	}
	filter.SetNoNewPrivsBit(true)

	// Load seccomp filters into the kernel.
	err := filter.Load()
	if err != nil {
		color.Set(color.FgRed)
		l.Printf("ScmpFilter.Load (1): ", err)
		color.Unset()
		os.Exit(1)
	}

	// Inform the parent that I'm ready to continue.
	// Any code before this line code must use only non-traced system calls in
	// the filter because the tracer has not set up itself yet.
	// (traced syscalls will cause ENOSYS "function not implemented" error)
	syscall.Kill(os.Getpid(), syscall.SIGSTOP)

	// Now we have the working tracer parent.
	// Make kill() syscall to be traced as well for more sophisticated filtering.
	killSyscalls := []string{"kill", "killpg", "tkill", "tgkill"}
	for _, syscallName := range killSyscalls {
		scId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(scId, seccomp.ActTrace)
	}
	err = filter.Load()
	if err != nil {
		color.Set(color.FgRed)
		l.Printf("ScmpFilter.Load (2): ", err)
		color.Unset()
		os.Exit(1)
	}

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
