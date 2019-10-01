#! /bin/bash
echo "Building Backend.AI Python support wheels for the new kernel image..."
PLATFORM=manylinux2010_x86_64
UTIL_PYBIN=/opt/python/cp37-cp37m/bin/python

# The agent sets environment variables:
echo "Settings:"
echo "  SRC_IMAGE    = ${SRC_IMAGE}"
echo "  TARGET_IMAGE = ${TARGET_IMAGE}"
echo "  RUNTIME_PATH = ${RUNTIME_PATH}"
echo ""
echo "Generated dockerfile:"
echo "====="
echo "$BUILD_SCRIPT" | $UTIL_PYBIN -m base64 -d - > /home/work/Dockerfile
cat /home/work/Dockerfile
echo ""
echo "====="

cd /root

DOCKER_CREDS_FILE=/home/config/docker-creds.json
for registry in $(cat $DOCKER_CREDS_FILE | jq -r 'keys[]'); do
  username=$(cat $DOCKER_CREDS_FILE | jq -r "."\""$registry"\"".username")
  password=$(cat $DOCKER_CREDS_FILE | jq -e -r "."\""$registry"\"".password")
  if [ "$?" -eq 0 ]; then
    echo "Logging into docker registry: $registry"
    docker login -u "$username" -p "$password" "$registry" 2>/dev/null
  fi
done

set -e

# Detect the Python version used by the imported image
docker pull ${SRC_IMAGE}

echo ""
echo "Checking the target Python version..."
PYVER_FULL=$(docker run --rm -it "${SRC_IMAGE}" "${RUNTIME_PATH}" --version | cut -d ' ' -f 2)
PYVER_MAJOR=$(echo $PYVER_FULL | cut -d . -f 1)
PYVER_MINOR=$(echo $PYVER_FULL | cut -d . -f 2)
PYVER="${PYVER_MAJOR}${PYVER_MINOR}"
PYROOT="/opt/python/cp${PYVER}-cp${PYVER}m"
echo "Detected Python version: $PYVER"

echo ""
echo "Buildling wheels for the query/batch-mode execution with ipykernel support..."
# Compile wheels and make it compatible with manylinux2010
"${PYROOT}/bin/pip" wheel -r requirements.txt -w /home/work/wheelhouse/
for whl in /home/work/wheelhouse/*.whl; do
  auditwheel repair "$whl" --plat $PLATFORM -w /home/work/wheelhouse/
done

echo ""
echo "Buildling the converted Docker image and pushing it to the registry..."
# Convert the imported image into the Backend.AI-compatible kernel image
echo ".docker" > /home/work/.dockerignore
export DOCKER_BUILDKIT=1
docker build -t ${TARGET_IMAGE} -f /home/work/Dockerfile /home/work
docker push ${TARGET_IMAGE}
docker rmi ${SRC_IMAGE}

echo ""
echo "Completed image conversion."
echo "Please perform 'rescan-images' in the manager to use this image."
