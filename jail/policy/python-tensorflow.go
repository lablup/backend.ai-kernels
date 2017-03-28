package policy

import (
	"strings"
)

type PythonTensorFlowPolicy struct {
}

func (p *PythonTensorFlowPolicy) CheckPathOp(path string, op PathOps, mode int) bool {
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

func (p *PythonTensorFlowPolicy) GetExecAllowance() int {
	return 0
}

func (p *PythonTensorFlowPolicy) GetForkAllowance() int {
	// Note: pyzmq performs clone() twice on intialization.
	return -1
}

func (p *PythonTensorFlowPolicy) GetMaxChildProcs() uint {
	return 32
}

func (p *PythonTensorFlowPolicy) CheckPathExecutable(path string) bool {
	// TODO: implement
	return true
}

func (p *PythonTensorFlowPolicy) GetExtraEnvs() []string {
	return []string{}
}

func (p *PythonTensorFlowPolicy) GetPreservedEnvKeys() []string {
	return []string{
		"HOME", "PATH", "LANG",
		"PYENV_ROOT", "PYTHONPATH",
		"PYTHONUNBUFFERED",
		"MPLCONFIGDIR",
		"OPENBLAS_NUM_THREADS",
		"OMP_NUM_THREADS",
		// for nvidia-docker base image
		"CUDA_VERSION",
		"CUDA_PKG_VERSION",
		"LD_LIBRARY_PATH",
		"LD_PRELOAD",
	}
}

// vim: ts=4 sts=4 sw=4 noet
