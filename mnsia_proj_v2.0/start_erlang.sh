#!/bin/bash

# --- Configuration ---
# The directory where you want the terminals to open.
# This should be the root of your project.
TARGET_DIR="/home/csestudent/Desktop/bar/Proj"

# The host/IP address for your Erlang nodes.
HOST="132.72.81.60"

# An array of the node names (without the @hostname part).
NODES=("main" "worker1" "worker2" "worker3" "worker4")

# --- Script Logic ---
echo "🚀 Starting 5 Erlang nodes using rebar3..."

# Loop through each node name in the array
for node in "${NODES[@]}"
do
  # Construct the full node name (e.g., main@127.0.0.1)
  FULL_NAME="${node}@${HOST}"
  
  echo "    -> Opening terminal for ${FULL_NAME}"
  
  # Command to open a new gnome-terminal, change to the target directory,
  # and then start the named rebar3 shell.
  gnome-terminal -- bash -c "cd ${TARGET_DIR}; rebar3 shell --name ${FULL_NAME}; exec bash"
  
done

echo "✅ All terminals should be open."