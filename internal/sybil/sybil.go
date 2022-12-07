package sybil

import (
	"math"

	"downpour/internal/metainfo"
)

// Metadata for sybil attack
type SybilInfo struct {
	Identity    int
	NumIdentity int
	PieceWindow int
	PieceBegin  int64
	PieceEnd    int64
	OffsetBegin int64
	OffsetEnd   int64
	Length      int64
}

func New(id int, numId int, info *metainfo.Info) *SybilInfo {
	num_pieces   := int64(info.NumPieces)
	piece_length := int64(info.PieceLength)

	piece_window := int(math.Ceil(
					float64(info.NumPieces)/float64(numId)))
	piece_begin := int64(piece_window) * int64(id) // per piece
	piece_end   := int64(piece_window) * (int64(id) + 1)
	if piece_end > num_pieces {
		piece_end = num_pieces
	}
	offset_begin := piece_length * piece_begin
	offset_end   := piece_length * piece_end
	var length int64
	if offset_end > info.Files[0].Length {
		length = info.Files[0].Length - offset_begin
	} else {
		length = offset_end - offset_begin
	}

	return &SybilInfo{
		Identity:    id,
		NumIdentity: numId,
		PieceWindow: piece_window,
		PieceBegin:  piece_begin,
		PieceEnd:    piece_end,
		OffsetBegin: offset_begin,
		OffsetEnd:   offset_end,
		Length:      length,
	}
}


func (s *SybilInfo) ToPieceIndex(index uint32) uint32 {
	return index - uint32(s.PieceBegin)
}

func (s *SybilInfo) ToActualIndex(index uint32) uint32 {
	return index + uint32(s.PieceBegin)
}
