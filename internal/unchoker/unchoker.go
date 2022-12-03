package unchoker

import (
	"fmt"
	"math"
	"math/rand"
	"sort"
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
	SetUploadLimit(speed int)

	DownloadSpeed() int
	UploadSpeed() int
}

// New returns a new Unchoker.
func New(numUnchoked, numOptimisticUnchoked, speedLimitUpload int) *Unchoker {
	capacity := math.MaxInt
	if speedLimitUpload > 0 {
		capacity = speedLimitUpload
	}
	return &Unchoker{
		numUnchoked:             numUnchoked,
		numOptimisticUnchoked:   numOptimisticUnchoked,
		peersUnchoked:           make(map[Peer]struct{}, numUnchoked),
		peersUnchokedOptimistic: make(map[Peer]struct{}, numUnchoked),
		Capacity:                capacity,
		UnchokeRoundsThres:      2,
		Delta:                   0.2,
		Gamma:                   0.1,
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


// TickUnchoke must be called at every 10 seconds.
func (u *Unchoker) TickUnchoke(allPeers []Peer, torrentCompleted bool) {
	if torrentCompleted {
		// When completed, we switch to the naive unchoke
		u.TickVanillaUnchoke(allPeers, true)
	} else {
		u.TickTyrantUnchoke(allPeers)
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
func (u *Unchoker) TickTyrantUnchoke(allPeers []Peer) {

	// fmt.Println("------------- In Tyrant Unchoke() -------------")
	peers := u.candidatesUnchoke(allPeers)

	// Adjust
	for _, pe := range peers {
		if pe.ChokingUs() { // When completed, everyone is choking us
			pe.SetUnchokedRounds(0)
			pe.SetUploadContribution(int(float32(pe.UploadContribution()) * (1 + u.Delta)))
		} else {
			pe.SetEstimatedReciprocation()
			pe.SetUnchokedRounds((pe.UnchokedRounds() + 1) % u.UnchokeRoundsThres)
			if pe.UnchokedRounds() == 0 {
				pe.SetUploadContribution(int(float32(pe.UploadContribution()) * (1 - u.Gamma)))
			}
		}
	}

	u.sortPeersByRatio(peers)

	// bugdet of upload speed
	var budget int = 0
	var i int

	// Capacity is the total upload limit (int, KB)
	for i = 0; i < len(peers) && budget < u.Capacity; i++ {
		fmt.Printf("unchoked peer %d, budget needed %d, cap %d\n", i, peers[i].UploadContribution(), u.Capacity)
		if budget + peers[i].UploadContribution() > u.Capacity{
			// // FIXME not working?
			// fmt.Printf(">>>>>>>>>> %d\n", u.Capacity - budget)
			uc := u.Capacity - budget
			peers[i].SetUploadLimit(uc) // take a shot
			budget += uc
			i += 1
			break
		}
		budget = budget + peers[i].UploadContribution()
		// We adjust token bucket size for a peer before unchoke it
		// to minimize allocation overheads
		peers[i].SetUploadLimit(peers[i].UploadContribution())
		u.unchokePeer(peers[i])
	}


	peers = peers[i:]
	for _, pe := range peers {
		u.chokePeer(pe)
	}
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

func (u *Unchoker) TickVanillaUnchoke(allPeers []Peer, torrentCompleted bool) {
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
