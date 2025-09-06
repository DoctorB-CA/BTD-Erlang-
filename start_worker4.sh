#!/bin/bash
# Script to start WORKER 4 for the BTD game.
# Run this on the worker4 PC (132.72.81.224).

echo "🚀 Starting WORKER 4 node..."
ERL_FLAGS="-setcookie btd_game_cookie" rebar3 shell --name worker4@132.72.81.224
