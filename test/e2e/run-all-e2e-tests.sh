#!/bin/bash

set -e
cd "$(dirname "$0")"

for test in *.exp; do
    ./"$test"
done 