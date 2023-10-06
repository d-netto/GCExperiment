#!/bin/bash

# 1. Build the project
rm -rf build
mkdir build
cd build
cmake ..
make -j

# 2. Run the tests and check if at least one test failed
for FILE in ../test/*.java; do
    echo "Running test $FILE"
    ./src/interpreter $FILE > $FILE.ll
    clang++ $FILE.ll -O2 -o $FILE.bin
    ./$FILE.bin > $FILE.result
    # replace .java extension with .out extension
    OUTFILE=${FILE/.java/.out}
    diff $FILE.result $OUTFILE
    if [ $? -eq 0 ]; then
        echo "Test $FILE passed"
    else
        echo "Test $FILE failed"
    fi
done