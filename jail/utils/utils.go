package utils

import (
	"fmt"
	"os"
	"bytes"
	"strings"
	"path"
	"syscall"
)

func FilterEnvs(envs []string, preservedKeys []string) []string {
	filteredEnvs := []string{}
	for _, entry := range envs {
		var kept bool = false
		for _, key := range preservedKeys {
			if strings.HasPrefix(entry, key+"=") {
				kept = true
				break
			}
		}
		if kept {
			filteredEnvs = append(filteredEnvs, entry)
		}
	}
	return filteredEnvs
}

func GetExecutable(pid int) (string, error) {
	const deletedTag = " (deleted)"
	execPath, err := os.Readlink(fmt.Sprintf("/proc/%d/exe", pid))
	if err != nil {
		return execPath, err
	}
	execPath = strings.TrimSuffix(execPath, deletedTag)
	execPath = strings.TrimPrefix(execPath, deletedTag)
	switch execPath {
	case "/bin/sh", "/bin/bash", "/bin/dash":
		rawData := make([]byte, 1024)
		file, err := os.Open(fmt.Sprintf("/proc/%d/cmdline", pid))
		if err != nil {
			return execPath, err
		}
		file.Read(rawData)
		file.Close()
		data := string(rawData[:])
		cmd := strings.Split(data, "\x00")
		if !path.IsAbs(cmd[1]) && cmd[1] != "/usr/bin/env" {
			cwd, err := os.Readlink(fmt.Sprintf("/proc/%d/cwd", pid))
			if err != nil {
				return execPath, err
			}
			execPath = path.Join(cwd, cmd[1])
		} else {
			execPath = cmd[1]
		}
	}
	return execPath, nil
}

func ReadString(pid int, addr uintptr) string {
	out := make([]byte, syscall.PathMax)
	syscall.PtracePeekData(pid, addr, out)
	// Try to find the index of first null character
	length := bytes.IndexByte(out, 0)
	if length == -1 {
		length = syscall.PathMax
	}
	return string(out[:length])
}

func GetAbsPathAs(path_ string, pid int) string {
	if path.IsAbs(path_) {
		return path.Clean(path_)
	} else {
		pwdPath := fmt.Sprintf("/proc/%d/cwd", pid)
		pwd, _ := os.Readlink(pwdPath)
		path_ = path.Clean(path_)
		return path.Join(pwd, path_)
	}
}


// vim: ts=4 sts=4 sw=4 noet
