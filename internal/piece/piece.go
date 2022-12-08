package piece

import (
	"fmt"
	"bytes"
	"hash"

	"downpour/internal/allocator"
	"downpour/internal/filesection"
	"downpour/internal/metainfo"
	"downpour/internal/sybil"

	"golang.org/x/exp/constraints"
)

// BlockSize is the size of smallest piece data that we are going to request from peers.
const BlockSize = 16 * 1024

// Piece of a torrent.
type Piece struct {
	Index   uint32            // index in torrent
	Length  uint32            // always equal to Info.PieceLength except last piece
	Data    filesection.Piece // the place to write downloaded bytes
	Hash    []byte
	Writing bool
	Done    bool
}

// Block is part of a Piece that is specified in peerprotocol.Request messages.
type Block struct {
	Begin  uint32 // Offset in piece
	Length uint32 // Cannot exceed BlockSize. It's shorter for last block or if the next file is a padding file.
}

// NewPieces returns a slice of Pieces by mapping files to the pieces.
func NewPieces(info *metainfo.Info, files []allocator.File, sybil *sybil.SybilInfo) []Piece {
	var (
		fileIndex  int   // index of the current file in torrent
		fileLength int64 // length of the file in fileIndex
		fileEnd    int64 // absolute position of end of the file among all pieces
		fileOffset int64 // offset in file: [0, fileLength)
	)

	nextFile := func() {
		fileIndex++
		fileLength = info.Files[fileIndex].Length
		fileEnd += fileLength
		fileOffset = 0
	}

	// Init first file
	fileIndex = -1
	nextFile()



	// hmmm
	// k := int64(math.Ceil(float64(info.NumPieces)/float64(numIdentity)))
	// piece_begin := k * identity // per piece
	// piece_end   := k * (identity + 1)
	// if piece_end > info.NumPieces {
		// piece_end = info.NumPieces
	// }
	// offset_begin := info.PieceLength * piece_begin
	// offset_end   := info.PieceLength * piece_end
	// var length int64
	// if offset_end > info.Files[0].Length {
		// length = info.Files[0].Length - offset_begin
	// } else {
		// length = offset_end - offset_end
	// }

	// Suppose we only download one file

	// if sybil == nil {
		// fmt.Printf("sybil is nil !!\n")
	// }

	fileOffset = 0
	fileLength = sybil.Length
	fileLeft := func() int64 { return fileLength - fileOffset }

	// Construct pieces
	var total int64
	pieces := make([]Piece, info.NumPieces)
	pieceBegin := uint32(sybil.PieceBegin)
	pieceEnd := uint32(sybil.PieceEnd)
	// for i := uint32(0); i < info.NumPieces; i++ {
	for i := pieceBegin; i < pieceEnd; i++ {
		p := Piece{
			Index: i,
			Hash:  info.PieceHash(i),
		}

		var sections filesection.Piece

		// Construct p.Files
		var pieceOffset uint32
		pieceLeft := func() uint32 { return info.PieceLength - pieceOffset }
		for left := pieceLeft(); left > 0; {
			n := uint32(min(int64(left), fileLeft())) // number of bytes to write

			file := filesection.FileSection{
				File:    files[fileIndex].Storage,
				Offset:  fileOffset,
				Length:  int64(n),
				Name:    files[fileIndex].Name,
				Padding: files[fileIndex].Padding,
			}
			sections = append(sections, file)

			left -= n
			p.Length += n
			pieceOffset += n
			fileOffset += int64(n)
			total += int64(n)

			// if total == info.Length {
			fmt.Printf("total: %d, sybil.Length: %d, info.Length %d\n",
		total, sybil.Length, info.Length)
			fmt.Printf("piece start %d, piece end %d, info numpiece %d\n",
		sybil.PieceBegin, sybil.PieceEnd, info.NumPieces)
			if total == sybil.Length {
				break
			}
			if fileLeft() == 0 {
				nextFile()
				panic("Should not have the next file")
			}
		}

		p.Data = sections
		pieces[i] = p
	}
	return pieces
}

// numBlocks returns the number of blocks in the piece.
// The calculation is only correct when there is no padding in piece.
// It is only used in per-allocation of blocks slice in CalculateBlocks().
func (p *Piece) numBlocks() int {
	div, mod := divmod(p.Length, BlockSize)
	numBlocks := div
	if mod != 0 {
		numBlocks++
	}
	return int(numBlocks)
}

func (p *Piece) CalculateBlocks() []Block {
	return p.calculateBlocks(BlockSize)
}

func (p *Piece) calculateBlocks(blockSize uint32) []Block {
	blocks := make([]Block, 0, p.numBlocks())

	secIndex := 0
	sec := p.Data[secIndex]
	blk := Block{
		Begin:  0,
		Length: 0,
	}
	pieceOffset := uint32(0)
	secOffset := uint32(0)
	blkLeft := func() uint32 { return blockSize - blk.Length }
	secLeft := func() uint32 { return uint32(sec.Length) - secOffset }
	nextBlock := func() {
		if blk.Length == 0 {
			return
		}
		blocks = append(blocks, blk)
		blk.Begin = pieceOffset
		blk.Length = 0
	}
	hasNextSection := true
	nextSection := func() {
		secIndex++
		if secIndex == len(p.Data) {
			hasNextSection = false
			return
		}
		secOffset = 0
		sec = p.Data[secIndex]
	}
	for hasNextSection {
		if sec.Padding {
			pieceOffset += uint32(sec.Length)
			nextBlock()
			nextSection()
			continue
		}
		n := min(secLeft(), blkLeft())
		blk.Length += n
		pieceOffset += n
		secOffset += n
		if blkLeft() == 0 {
			nextBlock()
		}
		if secLeft() == 0 {
			nextSection()
		}
	}
	nextBlock()
	return blocks
}

// VerifyHash returns true if hash of piece data in buffer `buf` matches the hash of Piece.
func (p *Piece) VerifyHash(buf []byte, h hash.Hash) bool {
	if uint32(len(buf)) != p.Length {
		return false
	}
	_, _ = h.Write(buf)
	sum := h.Sum(nil)
	return bytes.Equal(sum, p.Hash)
}

func min[T constraints.Ordered](a, b T) T {
	if a < b {
		return a
	}
	return b
}

func divmod[T constraints.Unsigned](a, b T) (T, T) { return a / b, a % b }
