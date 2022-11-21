#!/bin/sh

# Prereq: sudo apt install bittornado ctorrent

# Fire up tracker
bttrack --port 1337 --dfile ~/.bttrack/dstate --logfile ~/.bttrack/tracker.log --nat_check 0 --scrape_allowed full

# Makefile
# all:
	# -@rm test.img
	# dd of=test.img bs=1G seek=1 count=0

# Create a torrent file
ctorrent -t -u "http://pc13.cloudlab.umass.edu:1337/announce" -s test.torrent test.img

# Start seeding
ctorrent test.torrent
