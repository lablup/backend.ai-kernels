package policy

type PythonPolicy struct {
}

func (p *PythonPolicy) CheckPathAccessible(path string, perm Permission) bool {
	// TODO: implement
	return true
}

func (p *PythonPolicy) GetExecAllowance() int {
	return 0
}

func (p *PythonPolicy) GetForkAllowance() int {
	// Note: pyzmq performs clone() twice on intialization.
	return -1
}

func (p *PythonPolicy) GetMaxChildProcs() uint {
	return 32
}

func (p *PythonPolicy) CheckPathExecutable(path string) bool {
	// TODO: implement
	return true
}

func (p *PythonPolicy) GetExtraEnvs() []string {
	return []string{}
}

func (p *PythonPolicy) GetPreservedEnvKeys() []string {
	return []string{"HOME", "PATH", "PYENV_ROOT", "PYTHONPATH", "MPLCONFIGDIR"}
}

// vim: ts=4 sts=4 sw=4 noet
