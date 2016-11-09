from _cuda_query import ffi, lib
import argparse
import sys


def cuda_ver():
    ver = ffi.new('int *')
    ret = lib.cudaRuntimeGetVersion(ver)
    if ret != 0:
        print('CUDA returned error. ({})'.format(ret), file=sys.stderr)
        sys.exit(1)
    major = ver[0] // 1000
    minor = (ver[0] % 1000) // 10
    print('{}.{}'.format(major, minor), end='')

def cudnn_ver():
    ver = lib.cudnnGetVersion()
    major = ver // 1000
    minor = (ver % 1000) // 10
    #print('{}.{}'.format(major, minor), end='')
    print('{}'.format(major), end='')

def compute_caps():
    count = ffi.new('int *')
    prop = ffi.new('struct cudaDeviceProp *')
    capabilities = set()
    ret = lib.cudaGetDeviceCount(count)
    if ret != 0:
        print('No CUDA devices detected.', file=sys.stderr)
        sys.exit(1)
    for dev in range(count[0]):
        lib.cudaGetDeviceProperties(prop, dev)
        capabilities.add('{}.{}'.format(prop.major, prop.minor))
    print(','.join(sorted(capabilities)), end='')


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.set_defaults(func=None)
    group = parser.add_mutually_exclusive_group()
    group.add_argument('--cuda-ver', dest='func', action='store_const', const=cuda_ver)
    group.add_argument('--cudnn-ver', dest='func', action='store_const', const=cudnn_ver)
    group.add_argument('--compute-caps', dest='func', action='store_const', const=compute_caps)
    args = parser.parse_args()
    if args.func is None:
        print('No function specified.', file=sys.stderr)
        sys.exit(1)
    else:
        args.func()
    sys.exit(0)
