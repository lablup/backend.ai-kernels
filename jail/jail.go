package main

/* This ptrace-based jail assumes linux/amd64 platforms. */

import (
    "fmt"
    "os"
    "syscall"
)

func main() {
    // Command-line usage:
    // ./jail <jail_id> <child_args ...>
    //
    // Example: ./jail abc123 /bin/ls wow
    args := os.Args[2:]
    cwd, _ := os.Getwd()
    if len(args) < 1 {
        fmt.Println("Not enough arguments.")
        return
    }
    pid, err := syscall.ForkExec(args[0], args, &syscall.ProcAttr{
        cwd,
        []string{},
        []uintptr{0, 1, 2},
        &syscall.SysProcAttr{ Ptrace: true },
    })
    if err != nil {
        fmt.Println("ForkExec:", err)
        return
    }
    proc, _ := os.FindProcess(pid)
    proc.Wait()
    syscall.PtraceSetOptions(pid, syscall.PTRACE_O_TRACESYSGOOD)
    count := 0
    for {
        syscall.PtraceSyscall(pid, 0)
        var state syscall.WaitStatus
        syscall.Wait4(pid, &state, 0, nil)
        if state.Exited() {
            break
        }
        if state.Stopped() && (state.StopSignal() & 0x80 != 0) {
            var regs syscall.PtraceRegs
            syscall.PtraceGetRegs(pid, &regs)
            fmt.Printf("syscall %d\n", regs.Orig_rax)
        }
        count ++
    }
}

// vim: ts=8 sts=4 sw=4 et
