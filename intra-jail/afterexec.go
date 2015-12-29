package main

import (
    "os"
    "log"
    "syscall"
    seccomp "github.com/seccomp/libseccomp-golang"
)

func main() {
    l := log.New(os.Stderr, "", 0)
    arch, _ := seccomp.GetNativeArch()
    filter, _ := seccomp.NewFilter(seccomp.ActAllow)
    forkSid, _ := seccomp.GetSyscallFromNameByArch("fork", arch)
    cloneSid, _ := seccomp.GetSyscallFromNameByArch("clone", arch)
    filter.AddRuleExact(forkSid, seccomp.ActKill)
    filter.AddRuleExact(cloneSid, seccomp.ActKill)
    filter.SetNoNewPrivsBit(true)
    err := filter.Load()
    if err != nil {
        l.Fatal("ScmpFilter.Load: ", err)
    }
    // Replace myself with the language runtime.
    syscall.Exec(os.Args[1], os.Args[1:], os.Environ())
}
