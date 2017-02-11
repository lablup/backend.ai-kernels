package policy

import (
	"strings"
)

type JuliaPolicy struct {
}

func (p *JuliaPolicy) CheckPathOp(path string, op PathOps, mode int) bool {
	var allow bool
	switch op {
	case OP_CHMOD:
		allow = false
		for _, prefix := range WhitelistPaths[op] {
			if strings.HasPrefix(path, prefix) {
				allow = true
				break
			}
		}
	default:
		allow = true
	}
	return allow
}

func (p *JuliaPolicy) GetExecAllowance() int {
	return 0
}

func (p *JuliaPolicy) GetForkAllowance() int {
	return -1
}

func (p *JuliaPolicy) GetMaxChildProcs() uint {
	return 32
}

func (p *JuliaPolicy) CheckPathExecutable(path string) bool {
	return true
}

func (p *JuliaPolicy) GetExtraEnvs() []string {
	return []string{}
}

func (p *JuliaPolicy) GetPreservedEnvKeys() []string {
	return []string{
		"HOME", "PATH", "LANG",
		"PYENV_ROOT", "PYTHONPATH",
		"PYTHONUNBUFFERED",
		"JULIA_CPU_CORES",
		"JULIA_PKGDIR",
		"OPENBLAS_NUM_THREADS",
		"MPLCONFIGDIR",
		"LD_PRELOAD",
	}
}

// vim: ts=4 sts=4 sw=4 noet
