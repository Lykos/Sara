#!/bin/bash

set -o xtrace

if [ "$#" -ne 2 ]; then
    echo "Illegal number of parameters"
    exit 1
fi
LIB_PREFIX="src/lib"
TEST_PREFIX="tests"

old_module=$1
new_module=$2
old_file=${old_module//.//}.hs
new_file=${new_module//.//}.hs
sed_command="sed -i s/$old_module/$new_module/g"

if [ -a $LIB_PREFIX/$old_file ]; then
    git mv $LIB_PREFIX/{$old_file,$new_file}
elif [ -a $TEST_PREFIX/$old_file ]; then
    git mv $TEST_PREFIX/{$old_file,$new_file}
else
    echo "File $old_file neither exists in $LIB_PREFIX nor $TEST_PREFIX."
    exit 1
fi
find -type f -name '*.hs' -exec $sed_command {} \;
$sed_command Sara.cabal
