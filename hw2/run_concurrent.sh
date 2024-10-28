#!/usr/bin/env bash

# Compile code
erlc main.erl util.erl

# Run my program.
input=$1

commands=""

files=()
while IFS= read -r line || [ -n "$line" ];
do
    windows_clean_line=$(echo $line | tr -d '\r')
    args=($windows_clean_line)
    case ${args[0]} in
        d)
            commands+="main:start_dir_service(${args[1]}, ${args[2]}), timer:sleep(1000)"
            ;;
        c)  
            commands+=", main:create('${args[1]}'), timer:sleep(2000)"
            ;;
        g)
            temp=${args[1]}
            temp="${temp#\'}"
            temp="${temp%\'}"
            files+=($temp)
            commands+=", main:get('${args[1]}'), timer:sleep(2000)"
            ;;
        i)
            commands+=", main:deactivate('${args[1]}'), timer:sleep(1000)"

            ;;
        q)
            commands+=", main:quit()."
            ;;
    esac
done < "$input"

# echo $commands
sleep 1
erl -noshell -deatached -eval "$commands"
sleep 1


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