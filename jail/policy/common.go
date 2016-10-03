package policy

import seccomp "github.com/seccomp/libseccomp-golang"
import "syscall"

var TracedSyscalls []string
var AllowedSyscalls []string
var ConditionallyAllowedSyscalls map[string]seccomp.ScmpCondition
var WhitelistPaths map[PathOps][]string

func init() {

	WhitelistPaths = map[PathOps][]string{
		OP_CHMOD: []string{"/home/work/", "/tmp/"},
	}

	// Following syscalls are intercepted by our ptrace-based tracer.
	// The tracer will implement its own policies, optinally by inspecting
	// the arguments in the registers.
	TracedSyscalls = []string{
		// 1st param is filename/path
		"stat",
		"lstat",
		"statfs",
		"readlink",
		"unlink",
		"rmdir",
		"truncate",
		"access", // 2nd param is mode
		"creat",  // 2nd param is mode
		"mkdir",  // 2nd param is mode
		"chmod",  // 2nd param is mode
		"open",   // 3rd param is mode
		// 2nd param is filename/path
		"readlinkat",
		"unlinkat",
		"fchmodat",  // 3rd param is mode
		"faccessat", // 3rd param is mode
		"mkdirat",   // 3rd param is mode
		"openat",    // 4th param is mode
		// 1st & 2nd params are filename/paths
		"rename",
		// 2nd & 4th params are filename/paths
		"renameat",
		// traced by ptrace exec/fork/clone
		"fork",
		"vfork",
		"clone",
		"execve",
	}

	// Following syscalls are conditionally allowed.
	ConditionallyAllowedSyscalls = map[string]seccomp.ScmpCondition{
		"kill": {1, seccomp.CompareEqual, uint64(syscall.SIGSTOP), 0},
	}

	// Following syscalls are blindly allowed.
	// IMPORTANT: ptrace MUST NOT be included!
	AllowedSyscalls = []string{
		// blindly allowed
		"read",
		"recv",
		"recvfrom",
		"recvmsg",
		"write",
		"sendfile",
		"sendto",
		"sendmsg",
		"close",
		"fstat",
		"fstatfs",
		"pread64",
		"pwrite64",
		"mmap",
		"mprotect",
		"munmap",
		"mlock",
		"munlock",
		"mlockall",
		"munlockall",
		"brk",
		"lseek",
		"getdents",
		"dup",
		"dup2",
		"chdir",
		"fchdir",
		"flock",
		"fsync",
		"fdatasync",
		"ftruncate",
		"tkill",
		"tgkill",
		"rt_sigaction",
		"rt_sigprocmask",
		"rt_sigreturn",
		"sigaltstack",
		"arch_prctl",
		"prctl",
		"getrlimit",
		"set_tid_address",
		"clear_tid_address",
		"set_robust_list",
		"get_robust_list",
		"futex",
		"sched_getaffinity",
		"sched_setaffinity",
		"sched_getparam",
		"sched_getscheduler",
		"sched_setscheduler",
		"eventfd",
		"eventfd2",
		"setsockopt",
		"getsockopt",
		"getsockname",
		"getpeername",
		"bind",
		"listen",
		"gettid",
		"getuid",
		"geteuid",
		"getgid",
		"getegid",
		"getcwd",
		"socket",
		"socketpair",
		"connect",
		"accept",
		"shutdown",
		"pipe",
		"pipe2",
		"ioctl",
		"fcntl",
		"select",
		"poll",
		"epoll_create",
		"epoll_create1",
		"epoll_wait",
		"epoll_ctl",
		"exit",
		"exit_group",
		"wait4",
		"uname",
		"getrandom",
		// potentially replaced with VDSO
		"getpid",
		"getppid",
		"getpgid",
		"getpgrp",
		"getsid",
		"gettimeofday",
		"clock_gettime",
		"clock_getres",
		"clock_nanosleep",
	}
}

// vim: ts=4 sts=4 sw=4 noet
