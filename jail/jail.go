package main

/* This ptrace-based jail assumes linux/amd64 platforms. */

import (
    "os"
    "log"
    "syscall"
    "path"
    "sorna-repl/jail/policy"
    "github.com/kardianos/osext"
)

func main() {
    // Command-line usage:
    // ./jail <jail_id> <child_args ...>
    //
    // Example: ./jail abc123 /bin/ls wow
    l := log.New(os.Stderr, "", 0)
    l.Print("my pid: ", os.Getpid())
    if len(os.Args) < 2 {
        l.Print("You need to specify jail ID, absolute path to executable and its arguments.")
        l.Fatal("Error: Not enough command-line arguments.")
    }
    jail_id := os.Args[1]
    l.Print("Jail ID: ", jail_id)
    myExecPath, _ := osext.Executable()
    myPath, _ := path.Split(myExecPath)
    args := append([]string{path.Join(myPath, "intra-jail")}, os.Args[2:]...)
    policyInst, err := policy.GeneratePolicy(args[1])
    l.Printf("%#v\n", policyInst)
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
    l.Print("child-pid: ", pid)
    syscall.PtraceSetOptions(pid, syscall.PTRACE_O_TRACESYSGOOD)
    for {
        syscall.PtraceSyscall(pid, 0)
        var state syscall.WaitStatus
        syscall.Wait4(pid, &state, 0, nil)
        if state.Exited() {
            l.Print("child-exit-status: ", state.ExitStatus(), state.TrapCause())
            break
        } else if state.Stopped() && (state.StopSignal() & 0x80 != 0) {
            var regs syscall.PtraceRegs
            syscall.PtraceGetRegs(pid, &regs)
            syscallId := uint(regs.Orig_rax)
            syscallType := policy.GetSyscallType(syscallId)
            var allow bool
            switch syscallType {
            case policy.IO_OPEN, policy.IO_READ, policy.IO_WRITE:
                // TODO: extract path from syscall args
                allow = policyInst.AllowPath("...")
            default:
                allow = policyInst.AllowSyscall(syscallId)
            }
            if allow {
                l.Printf("Syscall %d is allowed.\n", syscallId)
            }
        } else {
            l.Print("child-update: ", state.ExitStatus(), state.TrapCause())
        }
    }
}

// vim: ts=8 sts=4 sw=4 et
