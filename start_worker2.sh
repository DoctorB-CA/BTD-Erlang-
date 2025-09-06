#!/bin/bash
# Script to start WORKER 2 for the BTD game.
# Run this on the worker2 PC (132.72.81.85).

echo "🚀 Starting WORKER 2 node..."
ERL_FLAGS="-setcookie btd_game_cookie" rebar3 shell --name worker2@132.72.81.85
