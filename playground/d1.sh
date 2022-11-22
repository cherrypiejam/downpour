#!/bin/sh

if [ "$1" == "debug" ]; then
    ../downpour -d dd -t torrents/local_test.torrent -c config/config.1.yaml -p 0 -o data/1
else
    ../downpour dd -t torrents/local_test.torrent -c config/config.1.yaml -p 0 -o data/1
fi

