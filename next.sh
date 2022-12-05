#!/bin/bash

set -e

if [ ! $# -eq 1 ]; then
    echo Need a problem number
    exit -1
fi

NEXT=$1
PADDED=$(printf "%02d\n" $NEXT)

PN=day$PADDED

cp -r template $PN
curl -b session=$(cat .session-cookie) https://adventofcode.com/2022/day/$NEXT/input > $PN/input.txt

pushd $PN
mv dayxx.cabal $PN.cabal
sed -i "s/dayxx/$PN/" $PN.cabal
cabal run
