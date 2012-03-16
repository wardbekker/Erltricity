#!/bin/sh

rebar compile skip-deps=true && erl -mnesia dir db_data -pa `pwd`/ebin `pwd`/deps/*/ebin -boot start_sasl +P 134217727 -run reloader
