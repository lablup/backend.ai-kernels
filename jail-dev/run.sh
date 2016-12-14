#! /bin/bash
scriptDir="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
repoPath=$(dirname $scriptDir)

# build if image is not present
imageName=$(docker images | grep jail-dev)
if [ -z "$imageName" ]; then
    docker build $repoPath/jail-dev -t jail-dev
fi

# run or start/attach the container
runningName=$(docker ps -a | grep jail-dev)
if [ -z "$runningName" ]; then
    docker run -i -t -v $repoPath:/root/workspace/src/sorna-repl --security-opt seccomp:unconfined --name jail-dev jail-dev
else
    docker start jail-dev && docker attach jail-dev
fi
