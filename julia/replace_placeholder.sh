#!/bin/bash
path="/opt/julia/packages/IJulia"
first_dir=$(ls -1 "$path" | head -1)
file="/usr/local/share/jupyter/kernels/julia-1.8/kernel.json"
old_string="PLACEHOLDER"
sed -i "s/$old_string/$first_dir/g" $file
