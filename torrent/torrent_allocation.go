package torrent

import (
	"fmt"

	"downpour/internal/allocator"
	"downpour/internal/bitfield"
	"downpour/internal/piece"
	"downpour/internal/piecepicker"
)

func (t *torrent) handleAllocationDone(al *allocator.Allocator) {
	if t.allocator != al {
		panic("invalid allocator")
	}
	t.allocator = nil

	if al.Error != nil {
		t.stop(fmt.Errorf("file allocation error: %s", al.Error))
		return
	}

	if t.files != nil {
		panic("files exist")
	}
	t.files = al.Files

	if t.pieces != nil {
		panic("pieces exists")
	}

	// TODO for range min <= i < max, specifiing range
	// parameter needed: #, which part
	pieces := piece.NewPieces(t.info, t.files, t.Sybil)
	if len(pieces) == 0 {
		t.stop(fmt.Errorf("torrent has zero pieces"))
		return
	}
	t.pieces = pieces

	for pe := range t.peers {
		pe.GenerateAndSendAllowedFastMessages(t.session.config.AllowedFastSet, t.info.NumPieces, t.infoHash, t.pieces)
	}

	if t.piecePicker != nil {
		panic("piece picker exists")
	}
	t.piecePicker = piecepicker.New2(t.pieces, t.session.config.EndgameMaxDuplicateDownloads, t.webseedSources, t.Sybil)

	for pe := range t.peers {
		pe.Bitfield = bitfield.New(t.info.NumPieces)
	}

	// If we already have bitfield from resume db, skip verification and start downloading.
	if t.bitfield != nil && !al.HasMissing {
		for i := uint32(0); i < t.bitfield.Len(); i++ {
			t.pieces[i].Done = t.bitfield.Test(i)
		}
		if t.checkCompletion() && t.stopAfterDownload {
			t.stopAndSetStoppedOnComplete()
			return
		}
		t.processQueuedMessages()
		t.addFixedPeers()
		t.startAcceptor()
		t.startAnnouncers()
		t.startPieceDownloaders()
		return
	}

	// No need to verify files if they didn't exist when we create them.
	if !al.HasExisting {
		t.mBitfield.Lock()
		t.bitfield = bitfield.New(t.info.NumPieces)
		t.mBitfield.Unlock()
		t.processQueuedMessages()
		t.addFixedPeers()
		t.startAcceptor()
		t.startAnnouncers()
		t.startPieceDownloaders()
		return
	}

	// Some files exists on the disk, need to verify pieces to create a correct bitfield.
	t.startVerifier()
}
