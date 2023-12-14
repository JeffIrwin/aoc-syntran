#!/bin/bash

# Provide year and date as single arg, e.g. "2023/13"
#
# TODO: arg count checking

mkdir -p $1
cp template.syntran $1/main.syntran

pushd $1

year=${1%%/*}
day=${1#*/}
echo "year = $year"
echo "day = $day"

# The cookies text file can be downloaded via firefox addin "cookies.txt"
curl --cookie ~/cookies.txt https://adventofcode.com/${year}/day/${day}/input -o input.txt

popd

