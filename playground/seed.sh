#!/bin/sh

if [ "$1" == "debug" ]; then
    ../downpour -d dd -t torrents/local_test.torrent -c config/config.0.yaml -p 0 -o data/seed --seed
else
    ../downpour dd -t torrents/local_test.torrent -c config/config.0.yaml -p 0 -o data/seed --seed
fi

