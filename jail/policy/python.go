package policy

type PythonPolicy struct {
}

func (p *PythonPolicy) AllowSyscall(syscallId uint) bool {
    // TODO: implement
    return true
}

func (p *PythonPolicy) AllowPath(path string) bool {
    // TODO: implement
    return true
}
