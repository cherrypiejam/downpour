#!/bin/sh

if [ "$1" == "debug" ]; then
    ./downpour -d dd -t playground/torrents/local_test.torrent -c playground/config/config.0.yaml -p 0 -o playground/data/seed --seed
else
    ./downpour dd -t playground/torrents/local_test.torrent -c playground/config/config.0.yaml -p 0 -o playground/data/seed --seed -dl 50 -ul 50
fi

