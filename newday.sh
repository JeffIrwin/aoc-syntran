#!/bin/bash

set -xe

# Provide year and date as single arg, e.g. "2023/1" or "2023/13"
#
# For days 1 through 9, make sure to use a single digit.  If you try "01" you
# will get a 404 error
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
#
# To save cookies, click the puzzle-shaped icon in the top-right of Firefox to
# access your extensions.  Then click on cookies.txt and "open extension" and
# finally "current site" (or "ALL").  Take whichever file gets exported for AOC
# and copy it to "~/cookies.txt", which is where the curl command below expects
# to find your cookies
#
curl --cookie ~/cookies.txt https://adventofcode.com/${year}/day/${day}/input -o input.txt

popd

