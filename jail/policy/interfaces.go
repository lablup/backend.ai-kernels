package policy

import (
    "fmt"
    "path"
)

type SandboxPolicy interface {
    AllowSyscall(syscallId uint) bool
    AllowPath(path string) bool
}

type SyscallType int
const (
    IO_OPEN SyscallType = iota
    IO_READ
    IO_WRITE
    IO_ETC
)

func GeneratePolicy(exec_path string) (SandboxPolicy, error) {
    _, exec_name := path.Split(exec_path)
    switch exec_name {
    case "python", "python3": return new(PythonPolicy), nil
    // TODO: add policies for other languages
    default: return nil, fmt.Errorf("Unsupported sandbox policy for %s.", exec_name)
    }
}

func GetSyscallType(syscallId uint) SyscallType {
    // TODO: implement
    return IO_OPEN
}
