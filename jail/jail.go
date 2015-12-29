package main

/* This ptrace-based jail assumes linux/amd64 platforms. */

import (
    "os"
    "log"
    "syscall"
    "path"
    "sorna-repl/jail/policy"
    "github.com/kardianos/osext"
    seccomp "github.com/seccomp/libseccomp-golang"
)

func main() {
    var allowedSyscallsSet = make(map[string]int)
    for _, name := range policy.AllowedSyscalls {
        allowedSyscallsSet[name] = 1
    }
    arch, _ := seccomp.GetNativeArch()
    var (
        id_Open, _ = seccomp.GetSyscallFromNameByArch("open", arch)
        id_Execve, _ = seccomp.GetSyscallFromNameByArch("execve", arch)
    )
    // Command-line usage:
    // ./jail <jail_id> <child_args ...>
    //
    // Example: ./jail abc123 /bin/ls wow
    l := log.New(os.Stderr, "", 0)
    if len(os.Args) < 2 {
        l.Print("You need to specify jail ID, absolute path to executable and its arguments.")
        l.Fatal("Error: Not enough command-line arguments.")
    }
    //jail_id := os.Args[1]
    //l.Print("Jail ID: ", jail_id)
    myExecPath, _ := osext.Executable()
    myPath, _ := path.Split(myExecPath)
    args := append([]string{path.Join(myPath, "intra-jail")}, os.Args[2:]...)
    policyInst, err := policy.GeneratePolicy(args[1])
    if err != nil {
        l.Fatal("GeneratePolicy: ", err)
    }
    cwd, _ := os.Getwd()
    pid, err := syscall.ForkExec(args[0], args, &syscall.ProcAttr{
        cwd,
        []string{},
        []uintptr{0, 1, 2},
        &syscall.SysProcAttr{ Ptrace: true },
    })
    if err != nil {
        l.Fatal("ForkExec: ", err)
    }
    syscall.PtraceSetOptions(pid, 1 << 7 /*PTRACE_O_TRACESECCOMP*/)
    var execveDone bool = false
    for {
        syscall.PtraceCont(pid, 0)
        var state syscall.WaitStatus
        syscall.Wait4(pid, &state, 0, nil)
        if state.Exited() {
            l.Print("child-exit-status: ", state.ExitStatus(), state.TrapCause())
            break
        } else if state.Stopped() && (state.StopSignal() & 7 /*PTRACE_EVENT_SECCOMP*/ != 0) {
            var regs syscall.PtraceRegs
            syscall.PtraceGetRegs(pid, &regs)
            syscallId := uint(regs.Orig_rax)
            allow := false
            switch seccomp.ScmpSyscall(syscallId) {
            case id_Open:
                // TODO: extract path from syscall args
                allow = policyInst.AllowPath("...")
            case id_Execve:
                // Allow only once!
                allow = !execveDone
                execveDone = true
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

// vim: ts=8 sts=4 sw=4 et
