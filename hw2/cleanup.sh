#!/usr/bin/env bash

# cleans up all nodes
pkill -x ds
pkill -x client
pkill -f beam.smp
sleep 1

# resets input and downloads
rm -r servers && rm -r downloads && mkdir servers && mkdir downloads

# Delete and recompile beam files
rm *.beam
erlc *.erl

mkdir servers/fs0
mkdir servers/fs1
mkdir servers/fs2