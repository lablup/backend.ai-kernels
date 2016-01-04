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
	arch, _ := seccomp.GetNativeArch()
	filter, _ := seccomp.NewFilter(seccomp.ActErrno.SetReturnCode(int16(syscall.EPERM)))
	for _, syscallName := range policy.TracedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(syscallId, seccomp.ActTrace)
	}
	for _, syscallName := range policy.AllowedSyscalls {
		syscallId, _ := seccomp.GetSyscallFromNameByArch(syscallName, arch)
		filter.AddRuleExact(syscallId, seccomp.ActAllow)
	}
	filter.SetNoNewPrivsBit(true)
	// Load seccomp filters into the kernel.
	l := log.New(os.Stderr, "", 0)
	err := filter.Load()
	if err != nil {
		l.Fatal("ScmpFilter.Load: ", err)
	}
	// Inform the parent that I'm ready to continue.
	syscall.Kill(os.Getpid(), syscall.SIGSTOP)
	// Replace myself with the language runtime.
	syscall.Exec(os.Args[1], os.Args[1:], os.Environ())
}

// vim: ts=4 sts=4 sw=4 noet
