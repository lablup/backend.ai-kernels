package policy

type PythonPolicy struct {
}

func (p *PythonPolicy) CheckPath(path string, perm Permission) bool {
	// TODO: implement
	return true
}

func (p *PythonPolicy) GetExecAllowance() uint {
	return 0
}

func (p *PythonPolicy) GetForkAllowance() uint {
	// pyzmq performs clone() twice on intialization.
	return 2
}

func (p *PythonPolicy) GetForkExecExceptionPaths() []string {
	return []string{}
}

func (p *PythonPolicy) GetExtraEnvs() []string {
	return []string{}
}

func (p *PythonPolicy) GetPreservedEnvKeys() []string {
	return []string{"HOME", "PATH", "PYENV_ROOT", "PYTHONPATH"}
}

// vim: ts=4 sts=4 sw=4 noet
