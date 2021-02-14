#!/bin/bash

set -e

THREADS=8
CONCURRENCY="16"


trap 'kill $(jobs -p)' EXIT

#  servant-beam servant-sentry-beam servant-psql-simple servant-sentry-psql-simple

for c in $CONCURRENCY; do
    for path in plaintext json db fortunes; do
        for p in servant-psql-simple servant-sentry-psql-simple; do
            sleep 5
            $p &
            PROG_PID=$!
            echo "Started $p in background (PID=$PROG_PID)"
            while ! nc -z localhost 7041; do echo "Waiting for port 7041 to come up..." && sleep 1; done
            TEST_NAME="$p-$path-$c" ./wrk/wrk -c $c -t $THREADS --latency -d 20 -s benchmark.lua http://localhost:7041/$path
            kill -9 $PROG_PID
            echo "$p killed (PID=$PROG_PID)"
            echo "---------------"
        done
        echo "================="
    done
done
