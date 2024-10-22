#!/usr/bin/env bash

# Compile code
erlc main.erl util.erl

# Run my program.
input=$1

files=()
while IFS= read -r line || [ -n "$line" ];
do
    windows_clean_line=$(echo $line | tr -d '\r')
    args=($windows_clean_line)
    case ${args[0]} in
        d)
            for ((i = 0 ; i < ${args[1]} ; i++ )); 
                do erl -noshell -detached -sname "fs$i@localhost" -setcookie foo
            done
            erl -noshell -detached -sname ds@localhost -setcookie foo -eval "main:start_dir_service(${args[1]}, ${args[2]})"
            sleep 1
            ;;
        c)  
            erl -noshell -detached -sname client@localhost -setcookie foo -eval "main:create('${args[1]}')"
            sleep 1
            pkill -f client@localhost
            sleep 1
            ;;
        g)
            temp=${args[1]}
            temp="${temp#\'}"
            temp="${temp%\'}"
            files+=($temp)
            erl -noshell -detached -sname client@localhost -setcookie foo -eval "main:get('${args[1]}')"
            sleep 1
            pkill -f client@localhost
            sleep 1
            ;;
        i)
            erl -noshell -detached -sname client@localhost -setcookie foo -eval "main:deactivate(${args[1]})"
            sleep 1
            pkill -f client@localhost
            sleep 1
            ;;
        q)
            erl -noshell -detached -sname client@localhost -setcookie foo -eval "main:quit()"
            sleep 1
            pkill -f client@localhost
            sleep 1
            ;;
        sleep)
            sleep ${args[1]}
    esac
done < "$input"

# for value in "${files[@]}"
# do
#     file1="input/$value"
#     file2="downloads/$value"
#     echo $file1
#     echo $file2
#     if cmp -s "$file1" "$file2"; then
#         printf "The files %s and %s are the same\n" "$file1" "$file2"
#     else
#         printf "The files %s and %s are different\n" "$file1" "$file2"
#     fi    
# done