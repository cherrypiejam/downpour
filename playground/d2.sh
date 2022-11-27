#!/bin/sh

if [ "$1" == "debug" ]; then
    ./downpour -d dd -t playground/torrents/local_test.torrent -c playground/config/config.2.yaml -p 0 -o playground/data/2
else
    ./downpour dd -t playground/torrents/local_test.torrent -c playground/config/config.2.yaml -p 0 -o playground/data/2
fi
