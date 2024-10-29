#!/usr/bin/env bash

# Clean and create output directories
rm -rf ./output
mkdir -p output/downloads
mkdir -p output/servers

rm crash_dump.log
rm ./logs/test.log
touch ./logs/test.log
echo "$(date)" > ./logs/test.log

./cleanup.sh

# Define servers
bad=("fs0" "fs1" "fs2" "fs3" "fs4" "fs5" "fs6" "fs7" "fs8" "fs9" "fs10")

# Create server directories
for b in "${bad[@]}"; do 
    mkdir -p "output/servers/$b"
done

# Check if argument 1 is provided
if [ -z "$1" ]; then
    echo "Fuck you."
    exit 1
fi

# Set run script based on argument 1
if [ "$1" == "c" ]; then
    run_script="./run_concurrent.sh"
elif [ "$1" == "d" ]; then
    run_script="./run_distributed.sh"
else
    echo "Error: Invalid argument '$1'. Use 'c' for concurrent or 'd' for distributed."
    exit 1
fi

# Check if argument 2 (test name) is provided
if [ -z "$2" ]; then
    echo "Error: Missing second argument. Please provide the test name."
    exit 1
fi

test_name=$2


# Run the script with the test name as an argument
$run_script "$test_name" > /dev/null

# Strip the file extension from test_name (if any) for comparison
test_name_no_ext="${test_name%.*}"

# Verify the recursive structure between expected output and actual output
expected_dir="${test_name_no_ext}_expected_output"
output_dir="./output"

if [ ! -d "$expected_dir" ]; then
    echo "Error: Expected output directory '$expected_dir' does not exist."
    exit 1
fi


# Function to compare directory structure
compare_structure() {
    local dir1="$1"
    local dir2="$2"

    # Use diff with the -r flag for a recursive directory comparison
    diff -r "$dir1" "$dir2"
    return $?
}


echo "Comparing directory structure of '$expected_dir' with '$output_dir'..."
if compare_structure "$expected_dir" "$output_dir"; then
    echo "Success: The directory structures match."
else
    echo "Error: The directory structures do not match."
    exit 1
fi
