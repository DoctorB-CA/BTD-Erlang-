#!/bin/bash

# A script to run an Erlang node for the proj application using rebar3.
# This version uses long names and IP addresses for robust cross-machine communication.
# Usage: ./run.sh [main|worker1|worker2|...]

# The name of the node, taken from the first argument. Defaults to 'main'.
NAME=${1:-main}

# The Erlang cookie for the cluster.
COOKIE="btd"

# Get the primary IP address of the machine. This is more robust than a hostname.
# `hostname -I` lists all IPs, `awk '{print $1}'` takes the first one.
IP_ADDRESS=$(hostname -I | awk '{print $1}')

# The full name of the node (e.g., main@192.168.1.100).
FULL_NAME="${NAME}@${IP_ADDRESS}"

echo "Starting node: ${FULL_NAME} with rebar3 shell"

# Run rebar3 shell.
# --name: Sets the long name of the node, required for communication across different machines.
# --setcookie: Sets the cookie for distributed communication.
rebar3 shell --name ${FULL_NAME} --setcookie ${COOKIE} --apps proj --apps proj
