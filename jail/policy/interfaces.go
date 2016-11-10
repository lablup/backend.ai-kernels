package policy

import (
	"path"
)

type PathOps int

const (
	OP_OPEN PathOps = iota
	OP_ACCESS
	OP_EXEC
	OP_STAT
	OP_CHMOD
)

type SandboxPolicy interface {
	// Should return a boolean representing if access to the path
	// with the given permission is allowed or not.
	CheckPathOp(path string, op PathOps, mode int) bool

	// Should return the number of maximum execv() syscalls.
	// If it returns -1, no limit is imposed.
	GetExecAllowance() int

	// Should return the number of maximum fork()/clone() syscalls.
	// If it returns -1, no limit is imposed.
	GetForkAllowance() int

	// Should return the maximum number of child processes and threads.
	GetMaxChildProcs() uint

	// Should return a boolean representing if executing the executable file in
	// the given path.  Here executing means calling execve().
	CheckPathExecutable(path string) bool

	// Should return additional environment key-value pairs.
	// They will be merged to environment variables of the user process.
	GetExtraEnvs() []string

	// Should return which environment variables are kept intact.
	GetPreservedEnvKeys() []string
}

func GeneratePolicy(exec_path string) (SandboxPolicy, error) {
	_, exec_name := path.Split(exec_path)
	switch exec_name {
	case "python-tensorflow":
		return new(PythonTensorFlowPolicy), nil
	case "python", "python2", "python3":
		return new(PythonPolicy), nil
	case "julia":
		return new(JuliaPolicy), nil
	case "git":
		return new(GitPolicy), nil
	default:
		return new(DefaultPolicy), nil
	}
}

// vim: ts=4 sts=4 sw=4 noet
