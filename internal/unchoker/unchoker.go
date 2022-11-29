package unchoker

import (
	"math/rand"
	"sort"
	"fmt"
)

// Unchoker implements an algorithm to select peers to unchoke based on their download speed.
type Unchoker struct {
	numUnchoked           int
	numOptimisticUnchoked int

	// Every 3rd round an optimistic unchoke logic is applied.
	round uint8

	peersUnchoked           map[Peer]struct{}
	peersUnchokedOptimistic map[Peer]struct{}

	// Upload Capacity
	Capacity int

	UnchokeRoundsThres int
	Delta float32
	Gamma float32
}

// Peer of a torrent.
type Peer interface {
	// Sends messages and set choking status of local peeer
	Choke()
	Unchoke()

	// Choking returns choke status of local peer
	Choking() bool

	// Interested returns interest status of remote peer
	Interested() bool

	// ChokingUs returns chocking status of remote peer
	ChokingUs() bool

	// SetOptimistic sets the uptimistic unchoke status of peer
	SetOptimistic(value bool)
	// OptimisticUnchoked returns the value previously set by SetOptimistic
	Optimistic() bool

	UnchokedRounds() int
	SetUnchokedRounds(r int)
	DownloadReciprocation() int
	SetEstimatedReciprocation()
	UploadContribution() int
	SetUploadContribution(speed int)

	DownloadSpeed() int
	UploadSpeed() int
}

// New returns a new Unchoker.
func New(numUnchoked, numOptimisticUnchoked int) *Unchoker {
	return &Unchoker{
		numUnchoked:             numUnchoked,
		numOptimisticUnchoked:   numOptimisticUnchoked,
		peersUnchoked:           make(map[Peer]struct{}, numUnchoked),
		peersUnchokedOptimistic: make(map[Peer]struct{}, numUnchoked),
		Capacity: 64,	// Constant values for now
		UnchokeRoundsThres: 3,
		Delta: 0.2,
		Gamma: 0.1,
	}
}

// HandleDisconnect must be called to remove the peer from internal indexes.
func (u *Unchoker) HandleDisconnect(pe Peer) {
	delete(u.peersUnchoked, pe)
	delete(u.peersUnchokedOptimistic, pe)
}

func (u *Unchoker) candidatesUnchoke(allPeers []Peer) []Peer {
	peers := allPeers[:0]
	for _, pe := range allPeers {
		if pe.Interested() {
			peers = append(peers, pe)
		}
	}
	return peers
}

func (u *Unchoker) sortPeers(peers []Peer, completed bool) {
	byUploadSpeed := func(i, j int) bool { return peers[i].UploadSpeed() > peers[j].UploadSpeed() }
	byDownloadSpeed := func(i, j int) bool { return peers[i].DownloadSpeed() > peers[j].DownloadSpeed() }
	if completed {
		sort.Slice(peers, byUploadSpeed)
	} else {
		sort.Slice(peers, byDownloadSpeed)
	}
}

// BitTyrant sort peers by d_p/u_p
func (u *Unchoker) sortPeersByRatio(peers []Peer) {
	byRatio := func(i, j int) bool {
		if peers[i].UploadContribution() == 0 {
			return true
		}
		return peers[i].DownloadReciprocation() / peers[i].UploadContribution() >
			   peers[j].DownloadReciprocation() / peers[j].UploadContribution()
	}
	sort.Slice(peers, byRatio)
}

// BitTyrant's periodic 'unchoke'
func (u *Unchoker) TickTyrantUnchoke(allPeers []Peer, torrentCompleted bool) {

	fmt.Println("------------- In Tyrant Unchoke() -------------")
	peers := u.candidatesUnchoke(allPeers)


	// update u_p based on rounds of being choked/unchoked
	// Only update peers that are unchoked
	// FIXME if peer p unchoked/choked US, we update
	for _, pe := range peers {
		if pe.ChokingUs() {
			pe.SetUnchokedRounds(0)
			pe.SetUploadContribution(int(float32(pe.UploadContribution()) * (1 + u.Delta)))
			// TODO set upload token bucket
		} else {
			pe.SetEstimatedReciprocation()
			pe.SetUnchokedRounds((pe.UnchokedRounds() + 1) % u.UnchokeRoundsThres)
			if pe.UnchokedRounds() == 0 {
				pe.SetUploadContribution(int(float32(pe.UploadContribution()) * (1 - u.Gamma)))
				// TODO set upload token bucket
			}
		}
	}

	u.sortPeersByRatio(peers)

	// Capacity is the total upload limit (int64, KB)
	// each time before unchoke, we update this peer's token bucket with it's upload speed (int64)
	// each peer has a token bucket for upload
	// estimated download speed is a Meter, to get the value, use rate1()

	// bugdet of upload speed
	var budget int = 0

	var i int
	for i = 0; i < len(peers) && budget < u.Capacity; i++ {
		if budget + peers[i].UploadContribution() > u.Capacity{
			break
		}
		budget = budget + peers[i].UploadContribution()
		u.unchokePeer(peers[i])
	}

	peers = peers[i:]

	for _, pe := range peers {
		u.chokePeer(pe)
	}
	// u.round = (u.round + 1) % 3
}

// TickUnchoke must be called at every 10 seconds.
func (u *Unchoker) TickUnchoke(allPeers []Peer, torrentCompleted bool) {
	optimistic := u.round == 0
	peers := u.candidatesUnchoke(allPeers)
	u.sortPeers(peers, torrentCompleted)
	var i, unchoked int
	for ; i < len(peers) && unchoked < u.numUnchoked; i++ {
		if !optimistic && peers[i].Optimistic() {
			continue
		}
		u.unchokePeer(peers[i])
		unchoked++
	}
	peers = peers[i:]
	if optimistic {
		for i = 0; i < u.numOptimisticUnchoked && len(peers) > 0; i++ {
			n := rand.Intn(len(peers)) // nolint: gosec
			pe := peers[n]
			u.optimisticUnchokePeer(pe)
			peers[n], peers = peers[len(peers)-1], peers[:len(peers)-1]
		}
	}
	for _, pe := range peers {
		u.chokePeer(pe)
	}
	u.round = (u.round + 1) % 3
}

func (u *Unchoker) chokePeer(pe Peer) {
	if pe.Choking() {
		return
	}
	pe.Choke()
	pe.SetOptimistic(false)
	delete(u.peersUnchoked, pe)
	delete(u.peersUnchokedOptimistic, pe)
}

func (u *Unchoker) unchokePeer(pe Peer) {
	if !pe.Choking() {
		if pe.Optimistic() {
			// Move into regular unchoked peers
			pe.SetOptimistic(false)
			delete(u.peersUnchokedOptimistic, pe)
			u.peersUnchoked[pe] = struct{}{}
		}
		return
	}
	pe.Unchoke()
	u.peersUnchoked[pe] = struct{}{}
	pe.SetOptimistic(false)
}

func (u *Unchoker) optimisticUnchokePeer(pe Peer) {
	if !pe.Choking() {
		if !pe.Optimistic() {
			// Move into optimistic unchoked peers
			pe.SetOptimistic(true)
			delete(u.peersUnchoked, pe)
			u.peersUnchokedOptimistic[pe] = struct{}{}
		}
		return
	}
	pe.Unchoke()
	u.peersUnchokedOptimistic[pe] = struct{}{}
	pe.SetOptimistic(true)
}

// FastUnchoke must be called when remote peer is interested.
// Remote peer is unchoked immediately if there are not enough unchoked peers.
// Without this function, remote peer would have to wait for next unchoke period.
func (u *Unchoker) FastUnchoke(pe Peer) {
	if pe.Choking() && pe.Interested() && len(u.peersUnchoked) < u.numUnchoked {
		u.unchokePeer(pe)
	}
	if pe.Choking() && pe.Interested() && len(u.peersUnchokedOptimistic) < u.numOptimisticUnchoked {
		u.optimisticUnchokePeer(pe)
	}
}
