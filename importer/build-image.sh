#! /bin/sh

# The agent sets environment variables:
#  - SRC_IMAGE
#  - TARGET_IMAGE
#  - BUILD_SCRIPT
#  - RUNTIME_PATH

PLATFORM=manylinux2010_x86_64
UTIL_PYBIN=/opt/python/cp37-cp37m/bin/python
cd /root

# Detect the Python version used by the imported image
docker pull ${SRC_IMAGE}
PYVER_FULL=$(docker run --rm -it ${SRC_IMAGE} ${RUNTIME_PATH} --version | cut -d ' ' -f 2)
PYVER_MAJOR=$(echo $PYVER_FULL | cut -d . -f 1)
PYVER_MINOR=$(echo $PYVER_FULL | cut -d . -f 2)
PYVER="${PYVER_MAJOR}${PYVER_MINOR}"
PYBIN="/opt/python/cp${PYVER}-cp${PYVER}m/bin/python"

# Compile wheels and make it compatible with manylinux2010
"${PYBIN}/pip" wheel -r requirements.txt -w /root/wheelhouse/
for whl in /root/wheelhouse/*.whl; do
  auditwheel repair "$whl" --plat $PLATFORM -w /root/wheelhouse/
done

# Convert the imported image into the Backend.AI-compatible kernel image
echo "$BUILD_SCRIPT" | $UTIL_PYBIN -m base64 -d - > /root/Dockerfile
docker build -t ${TARGET_IMAGE} -f /root/Dockerfile /root
docker push ${TARGET_IMAGE}
docker rmi ${SRC_IMAGE}
