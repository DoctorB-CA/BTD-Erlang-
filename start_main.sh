#!/bin/bash
# Script to start the MAIN node for the BTD game.
# Run this on the main PC (132.72.81.60).

echo "🚀 Starting MAIN node..."
ERL_FLAGS="-setcookie btd_game_cookie" rebar3 shell --name main@132.72.81.60
