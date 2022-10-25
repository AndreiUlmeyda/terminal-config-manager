#! /usr/bin/env bash

docker build -t terminal-config-manager-system-tests .
docker create terminal-config-manager-system-tests:latest

echo "done building the container"