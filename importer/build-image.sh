#! /bin/bash
echo "Building Backend.AI Python support wheels for the new kernel image..."
UTIL_PYBIN=/opt/python/cp38-cp38/bin/python
RUNTIME_TYPE="${RUNTIME_TYPE:-python}"

# The agent sets environment variables:
echo "Settings:"
echo "  SRC_IMAGE    = ${SRC_IMAGE}"
echo "  TARGET_IMAGE = ${TARGET_IMAGE}"
echo "  RUNTIME_PATH = ${RUNTIME_TYPE}"
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

case ${RUNTIME_TYPE} in
  python)

    echo ""
    echo "Checking the target Python version..."
    PYVER_FULL=$(docker run --rm -it "${SRC_IMAGE}" "${RUNTIME_PATH}" --version | cut -d ' ' -f 2)
    PYVER_MAJOR=$(echo $PYVER_FULL | cut -d . -f 1)
    PYVER_MINOR=$(echo $PYVER_FULL | cut -d . -f 2)
    PYVER="${PYVER_MAJOR}${PYVER_MINOR}"
    if [ "$PYVER_MAJOR" -ge 3 -a "$PYVER_MAJOR" -ge 8 ]; then
      # Python 3.8 and later no longer distinguish "pymalloc" build in the ABI flags.
      PYROOT="/opt/python/cp${PYVER}-cp${PYVER}"
    else
      PYROOT="/opt/python/cp${PYVER}-cp${PYVER}m"
    fi
    echo "Detected Python version: $PYVER"
    echo ""
    echo "Wheelhouse contents:"
    ls -lh /root/wheelhouse/
    mkdir /home/work/wheelhouse
    cp /root/wheelhouse/*.whl /home/work/wheelhouse/
    ;;

  app)
    echo ""
    echo "Nothing to install for pure app images."
    ;;

  *) echo "Unsupported runtime type: $RUNTIME_TYPE" exit 1 ;;
esac


echo ""
echo "Buildling the converted Docker image and pushing it to the registry..."
# Convert the imported image into the Backend.AI-compatible kernel image
echo ".docker" > /home/work/.dockerignore
export DOCKER_BUILDKIT=1
docker build -t ${TARGET_IMAGE} -f /home/work/Dockerfile /home/work

echo ""
echo "Pushing the converted image to the registry..."
docker push ${TARGET_IMAGE}
docker rmi ${SRC_IMAGE}

echo ""
echo "Completed image conversion."
echo "Please perform 'rescan-images' in the manager to use this image."
