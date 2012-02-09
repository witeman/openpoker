#!/bin/sh
# NOTE: mustache templates need \ because they are not awesome.
exec erl -pa ebin edit deps/*/ebin -boot start_sasl \
    +K true \
    +P 2097152 \
    +A 64 \
    -env ERL_MAX_PORTS 4096 \
    -env ERL_FULLSWEEP_AFTER 10 \
    -sname openpoker_dev \
    -s reloader \
    -mnesia dir db
