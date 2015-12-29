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
            syscallState := syscall.Errno(-int64(regs.Rax))
            if syscallState == syscall.ENOSYS {
                // When entering syscall
                syscallType := policy.GetSyscallType(syscallId)
                var allow bool
                switch syscallType {
                case policy.IO_OPEN:
                    // TODO: extract path from syscall args
                    allow = policyInst.AllowPath("...")
                default:
                    allow = true
                }
                if allow {
                    //seccomp.ScmpSyscall(syscallId).GetName()
                    syscallName, _ := seccomp.ScmpSyscall(syscallId).GetName()
                    if _, ok := allowedSyscallsSet[syscallName]; !ok {
                        l.Printf("unexpected syscall %s\n", syscallName)
                    }
                }
            }
        } else {
            //l.Print("child-update: ", state.ExitStatus(), state.TrapCause())
        }
    }
}

// vim: ts=8 sts=4 sw=4 et
