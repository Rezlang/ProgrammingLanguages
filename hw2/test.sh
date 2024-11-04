#!/usr/bin/env bash
run_both() {
    for ((i = 1 ; i <= 10 ; i++)); do
    echo "Running test $i concurrent"
    ./run.sh c "tests/test$i.txt"
    done
    for ((i = 1 ; i <= 10 ; i++)); do
    echo "Running test $i distributed"
    ./run.sh d "tests/test$i.txt"
    done


}

if [ -z $1 ]; then
    echo "Fuck your mother"
    exit
fi

if [ $1 != 'c' ] && [ $1 != 'd' ]; then
    echo "dickhead"
    run_both
    exit
fi

if [ $1 == 'c' ]; then
    mode="concurrent"
elif [ $1 == 'd' ]; then
    mode="distributed"
else
    mode="fuck youuuuuuuuuuu"
fi

for ((i = 1 ; i <= 10 ; i++)); do
echo "Running test $i $mode"
./run.sh $1 "tests/test$i.txt"
done
