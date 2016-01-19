package main

import (
	seccomp "github.com/seccomp/libseccomp-golang"
	"log"
	"os"
	"sorna-repl/jail/policy"
	"syscall"
)

func main() {
	syscall.RawSyscall(syscall.SYS_PRCTL, syscall.PR_SET_PTRACER, uintptr(os.Getppid()), 0)

	// Inform the parent that I'm ready to continue.
	// This must be called before setting up seccomp syscall filters,
	// as the filter disables use of ptrace.
	syscall.Kill(os.Getpid(), syscall.SIGSTOP)

	arch, _ := seccomp.GetNativeArch()
	filter, _ := seccomp.NewFilter(seccomp.ActTrace)
	for _, syscallName := range policy.TracedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(syscallId, seccomp.ActTrace)
	}
	for _, syscallName := range policy.AllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(syscallId, seccomp.ActAllow)
	}
	for syscallName, cond := range policy.ConditionallyAllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleConditional(syscallId, seccomp.ActAllow, []seccomp.ScmpCondition{cond})
	}
	filter.SetNoNewPrivsBit(true)
	// Load seccomp filters into the kernel.
	l := log.New(os.Stderr, "", 0)
	err := filter.Load()
	if err != nil {
		l.Fatal("ScmpFilter.Load: ", err)
	}
	// Replace myself with the language runtime.
	err = syscall.Exec(os.Args[1], os.Args[1:], os.Environ())
	l.Fatalf("Exec: %s\n", err)
}

// vim: ts=4 sts=4 sw=4 noet
