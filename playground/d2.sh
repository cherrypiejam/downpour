#!/bin/sh

if [ "$1" == "debug" ]; then
    ../downpour -d dd -t torrents/local_test.torrent -c config/config.2.yaml -p 0 -o data/2
else
    ../downpour dd -t torrents/local_test.torrent -c config/config.2.yaml -p 0 -o data/2
fi
