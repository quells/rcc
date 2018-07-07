#!/bin/bash

# Build project
cargo build
RCC=target/debug/rcc

# Keep track of progress
valid_success=0
valid_total=0
invalid_error=0
invalid_total=0

# Log each file on issue?
verbose=$1

# Run tests
for stage in samples/stage_*; do
    for filename in $stage/valid/*.c; do
        let valid_total++

        $RCC $filename 2> /dev/null
        ret_code=$?
        
        if [ $ret_code != 0 ]; then
            if [[ -n "$verbose" ]]; then
                printf "Failed to compile %s\n" $filename
            fi
        else
            let valid_success++
        fi
    done

    for filename in $stage/invalid/*.c; do
        let invalid_total++

        $RCC $filename 2> /dev/null
        ret_code=$?

        if [ $ret_code != 1 ]; then
            if [[ -n "$verbose" ]]; then
                printf "Failed to catch invalid program %s\n" $filename
            fi
        else
            let invalid_error++
        fi
    done
done

# Print results
printf "Compiled %d / %d valid programs\n" $valid_success $valid_total
printf "Caught errors in %d / %d invalid programs\n" $invalid_error $invalid_total
