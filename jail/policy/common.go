package policy

import seccomp "github.com/seccomp/libseccomp-golang"
import "syscall"

var TracedSyscalls []string
var AllowedSyscalls []string
var ConditionallyAllowedSyscalls map[string]seccomp.ScmpCondition
var WhitelistPaths map[PathOps][]string

// References when you are going to update this file:
//  - https://github.com/docker/docker/blob/master/docs/security/seccomp.md
//  - https://filippo.io/linux-syscall-table/

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
		"link",
		"rename",
		// 1st & 3rd params are filename/paths
		"symlink",
		"symlinkat",
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
		"readv",
		"preadv",
		"pread64",
		"readahead",
		"recv",
		"recvfrom",
		"recvmsg",
		"recvmmsg",
		"write",
		"writev",
		"pwritev",
		"pwrite64",
		"sendfile",
		"sendto",
		"sendmsg",
		"sendmmsg",
		"close",
		"fallocate",
		"fstat",
		"fstatfs",
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
		"getdents64",
		"alarm",
		"dup",
		"dup2",
		"dup3",
		"chdir",
		"fchdir",
		"flock",
		"fsync",
		"fdatasync",
		"sync",
		"syncfs",
		"ftruncate",
		"utimensat",
		"futimens",
		"utime",
		"utimes",
		"tee",
		"splice",
		"vmsplice",
		"tkill",
		"tgkill",
		"rt_sigaction",
		"rt_sigprocmask",
		"rt_sigreturn",
		"rt_sigpending",
		"rt_sigtimedwait",
		"rt_sigsuspend",
		"rt_sigqueueinfo",
		"rt_tgsigqueueinfo",
		"sigaltstack",
		"restart_syscall",
		"semget",
		"semat",
		"semctl",
		"semtimedop",
		"shmget",
		"shmat",
		"shmctl",
		"shmdt",
		"msgget",
		"msgsnd",
		"msgrcv",
		"msgctl",
		"mincore",
		"fadvise64",
		"madvise",
		"arch_prctl",
		"prctl",
		"getrlimit",
		"set_tid_address",
		"clear_tid_address",
		"set_thread_area",
		"get_thread_area",
		"set_robust_list",
		"get_robust_list",
		"futex",
		"sched_getaffinity",
		"sched_setaffinity",
		"sched_getparam",
		"sched_getscheduler",
		"sched_setscheduler",
		"sched_get_priority_max",
		"sched_get_priority_min",
		"sched_rr_get_interval",
		"getpriority",
		"getcpu",
		"eventfd",
		"eventfd2",
		"signalfd",
		"signalfd4",
		"timerfd_create",
		"timerfd_settime",
		"timerfd_gettime",
		"setsockopt",
		"getsockopt",
		"getsockname",
		"getpeername",
		"bind",
		"listen",
		"gettid",
		"getuid",
		"geteuid",
		"getreuid",
		"getresuid",
		"getgid",
		"getegid",
		"getregid",
		"getresgid",
		"getcwd",
		"socket",
		"socketpair",
		"connect",
		"accept",
		"accept4",
		"shutdown",
		"pipe",
		"pipe2",
		"ioctl",
		"fcntl",
		"inotify_init",
		"inotify1_init",
		"inotify_add_watch",
		"inotify_rm_watch",
		"select",
		"pselect",
		"pselect6",
		"poll",
		"ppoll",
		"epoll_create",
		"epoll_create1",
		"epoll_wait",
		"epoll_pwait",
		"epoll_ctl",
		"exit",
		"exit_group",
		"wait",
		"wait3",
		"wait4",
		"waitid",
		"waitpid",
		"uname",
		"getrandom",
		"timer_create",
		"timer_settime",
		"timer_gettime",
		"timer_getoverrun",
		"timer_delete",
		"nanosleep",
		"capget",
		"syslog",
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
