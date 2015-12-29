package main

import (
    "os"
    "log"
    "syscall"
    "sorna-repl/jail/policy"
    seccomp "github.com/seccomp/libseccomp-golang"
)

func main() {
    arch, _ := seccomp.GetNativeArch()
    filter, _ := seccomp.NewFilter(seccomp.ActKill)
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
    // Replace myself with the language runtime.
    syscall.Exec(os.Args[1], os.Args[1:], os.Environ())
}

// vim: ts=8 sts=4 sw=4 et
