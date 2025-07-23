#!/bin/bash

set -e
cd "$(dirname "$0")"

for test in *.tcl; do
    ./"$test"
done 