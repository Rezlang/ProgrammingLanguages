#!/usr/bin/env bash

if [ -z $1 ] || [ $1 != 'c' ] && [ $1 != 'd' ]; then
    echo "Fuck your mother"
    exit
fi

for ((i = 1 ; i <= 10 ; i++)); do
echo "Running test $i distributed"
./run.sh $1 "tests/test$i.txt"
done
