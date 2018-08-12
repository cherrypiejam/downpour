package announcer

import (
	"net"
	"sort"
	"time"
)

type peerAddr struct {
	*net.TCPAddr
	timestamp time.Time
}

// NextPeerAddr returns the next peer address to connect from its list.
// If no peer in list, blocks until next announce.
// Returns nil if announcer has stopped.
func (a *Announcer) NextPeerAddr() net.Addr {
	a.m.Lock()
	defer a.m.Unlock()

	for len(a.peerAddrs) == 0 {
		a.gotPeer.Wait()
		if a.done {
			return nil
		}
	}

	var p *peerAddr
	p, a.peerAddrs = a.peerAddrs[len(a.peerAddrs)-1], a.peerAddrs[:len(a.peerAddrs)-1] // pop back
	delete(a.peerAddrsMap, p.String())
	return p
}

func (a *Announcer) putPeerAddrs(addrs []*net.TCPAddr) {
	a.m.Lock()
	defer a.m.Unlock()

	now := time.Now()
	for _, ad := range addrs {
		// 0 port is invalid
		if ad.Port == 0 {
			continue
		}
		// Discard own client
		if ad.IP.IsLoopback() && ad.Port == a.Transfer.Port() {
			continue
		}
		key := ad.String()
		if p, ok := a.peerAddrsMap[key]; ok {
			p.timestamp = now
		} else {
			p = &peerAddr{
				TCPAddr:   ad,
				timestamp: now,
			}
			a.peerAddrsMap[key] = p
			a.peerAddrs = append(a.peerAddrs, p)
		}
	}
	sort.Slice(a.peerAddrs, func(i, j int) bool { return a.peerAddrs[i].timestamp.Before(a.peerAddrs[j].timestamp) })
	a.gotPeer.Signal()
}