#!/bin/sh

# port # is defined in tracker/config.yaml
./downpour torrent create -f data/seed/local_test.img -t "http://127.0.0.1:6969/announce" -o torrents/local_test.torrent

# start tracker
./chihaya --debug --config tracker/config.yaml

