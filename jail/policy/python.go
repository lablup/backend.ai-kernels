package policy

type PythonPolicy struct {
}

func (p *PythonPolicy) CheckPath(path string, perm Permission) bool {
	// TODO: implement
	return true
}

func (p *PythonPolicy) GetExecAllowance() uint {
	// In Python, we use a bootstrapping shell script
	// to initialize pyenv, requiring two execve() calls.
	return 2
}

func (p *PythonPolicy) GetExtraEnvs() []string {
	// Currently empty.
	return []string{}
}

// vim: ts=4 sts=4 sw=4 noet
