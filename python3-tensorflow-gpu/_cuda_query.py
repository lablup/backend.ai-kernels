from cffi import FFI
ffibuilder = FFI()

ffibuilder.set_source("_cuda_query",
    """
    #include <cuda_runtime.h>
    #include <cudnn.h>
    """,
    include_dirs=['/usr/local/cuda/include', '/usr/local/nvidia/include'],
    library_dirs=['/usr/local/cuda/lib64', '/usr/local/nvidia/lib64'],
    libraries=['cudart', 'cudnn'])

ffibuilder.cdef("""
    struct cudaDeviceProp {
        int major;
        int minor;
        ...;
    };
    int cudaDriverGetVersion(int *ver);
    int cudaRuntimeGetVersion(int *ver);
    int cudaGetDeviceCount(int *count);
    int cudaGetDeviceProperties(struct cudaDeviceProp *prop, int device);
    size_t cudnnGetVersion();
""")

if __name__ == "__main__":
    ffibuilder.compile(verbose=False)
