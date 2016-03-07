package policy

import (
	"path"
)

type Permission int
const (
	PERM_RD Permission = iota
	PERM_RW
)

type SandboxPolicy interface {
	// Should return a boolean representing if access to the path
	// with the given permission is allowed or not.
	CheckPathAccessible(path string, perm Permission) bool

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
	case "python", "python2", "python3":
		return new(PythonPolicy), nil
	// TODO: add policies for other languages
	default:
		return new(DefaultPolicy), nil
	}
}

// vim: ts=4 sts=4 sw=4 noet
