#!/bin/bash
path="/opt/julia/packages/IJulia"
first_dir=$(ls -1 "$path" | head -1)
if [ -z "$1" ]
then
    version="1.5"
else
    version=$1
fi
file="/usr/local/share/jupyter/kernels/julia-${version}/kernel.json"
old_string="PLACEHOLDER"
sed -i "s/$old_string/$first_dir/g" $file