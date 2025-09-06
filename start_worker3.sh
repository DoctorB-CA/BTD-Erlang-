#!/bin/bash
# Script to start WORKER 3 for the BTD game.
# Run this on the worker3 PC (132.72.80.185).

echo "🚀 Starting WORKER 3 node..."
ERL_FLAGS="-setcookie btd_game_cookie" rebar3 shell --name worker3@132.72.80.185
