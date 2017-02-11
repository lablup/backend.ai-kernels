package policy

import (
	"strings"
)

type PythonPolicy struct {
}

func (p *PythonPolicy) CheckPathOp(path string, op PathOps, mode int) bool {
	var allow bool
	switch op {
	case OP_CHMOD:
		allow = strings.HasPrefix(path, "/home/work/")
	default:
		allow = true
	}
	return allow
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
	return []string{
		"HOME", "PATH", "LANG",
		"PYENV_ROOT", "PYTHONPATH",
		"PYTHONUNBUFFERED",
		"MPLCONFIGDIR",
		"OPENBLAS_NUM_THREADS",
	}
}

// vim: ts=4 sts=4 sw=4 noet
