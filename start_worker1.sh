#!/bin/bash
# Script to start WORKER 1 for the BTD game.
# Run this on the worker1 PC (132.72.81.167).

echo "🚀 Starting WORKER 1 node..."
ERL_FLAGS="-setcookie btd_game_cookie" rebar3 shell --name worker1@132.72.81.167
