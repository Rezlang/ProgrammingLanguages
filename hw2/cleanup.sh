#!/usr/bin/env bash

# cleans up all nodes
pkill -f ds
pkill -f client
for ((i = 0 ; i < 20 ; i++ )); 
    do pkill -f fs$i
done

# resets input and downloads
rm -r servers && rm -r downloads && mkdir servers && mkdir downloads