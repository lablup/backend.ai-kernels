#define _GNU_SOURCE
#include <dlfcn.h>
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <stdint.h>
#include <string.h>
#include <ctype.h>

typedef long (*orig_sysconf_ftype)(int flag);

static int nproc_from_sysfs_cpuset()
{
    const size_t maxlen = 512;
    char line[maxlen];
    FILE *fin = fopen("/sys/fs/cgroup/cpuset/cpuset.cpus", "r");
    if (fin == NULL)
        return 0;

    if (fgets(line, maxlen, fin) == NULL)
        return 0;

    int result = 0;
    char *l = &line[0];
    char *ends = (l + (uintptr_t) strnlen(line, maxlen));
    do {
        char *endp;
        unsigned long int n = strtoul(l, &endp, 10);
        if (l == endp) {
            result = 0;
            break;
        }
        unsigned long int m = n;
        if (*endp == '-') {
            l = endp + 1;
            m = strtoul(l, &endp, 10);
            if (l == endp) {
                result = 0;
                break;
            }
        }
        result += (m - n + 1);
        l = endp;
        while (l < ends && (isspace(*l) || *l == ','))
            ++l;
    } while (l < ends);

    fclose(fin);
    return result;
}

long sysconf(int flag)
{
    orig_sysconf_ftype orig_sysconf;
    orig_sysconf = (orig_sysconf_ftype) dlsym(RTLD_NEXT, "sysconf");
    switch (flag) {
    case _SC_NPROCESSORS_ONLN:
    case _SC_NPROCESSORS_CONF:
        return nproc_from_sysfs_cpuset();
    default:
        break;
    }
    return orig_sysconf(flag);
}
