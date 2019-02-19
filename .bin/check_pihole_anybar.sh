#!/bin/bash
set -euo pipefail
export SERVER=10.82.251.124
export ANYBAR_PORT=1738
open -g -a AnyBar
if grep -q $SERVER /etc/resolv.conf && \
    dig google.com @$SERVER +time=3 >/dev/null; then
    echo -n green | nc -4u -w0 localhost $ANYBAR_PORT;
else
    echo -n red | nc -4u -w0 localhost $ANYBAR_PORT;
fi
