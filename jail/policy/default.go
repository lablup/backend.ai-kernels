package policy

type DefaultPolicy struct {
}

func (p *DefaultPolicy) CheckPathAccessible(path string, perm Permission) bool {
	return true
}

func (p *DefaultPolicy) GetExecAllowance() int {
	return 0
}

func (p *DefaultPolicy) GetForkAllowance() int {
	return -1
}

func (p *DefaultPolicy) CheckPathExecutable(path string) bool {
	return true
}

func (p *DefaultPolicy) GetExtraEnvs() []string {
	return []string{}
}

func (p *DefaultPolicy) GetPreservedEnvKeys() []string {
	return []string{"HOME", "PATH"}
}

// vim: ts=4 sts=4 sw=4 noet
