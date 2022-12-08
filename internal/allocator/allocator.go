package allocator

import (
	"fmt"

	"downpour/internal/metainfo"
	"downpour/internal/storage"
	"downpour/internal/sybil"
)

// Allocator allocates files on the disk.
type Allocator struct {
	Files       []File
	HasExisting bool
	HasMissing  bool
	Error       error

	closeC chan struct{}
	doneC  chan struct{}
}

// File on the disk.
type File struct {
	Storage storage.File
	Name    string
	Padding bool
}

// Progress about the allocation.
type Progress struct {
	AllocatedSize int64
}

// New returns a new Allocator.
func New() *Allocator {
	return &Allocator{
		closeC: make(chan struct{}),
		doneC:  make(chan struct{}),
	}
}

// Close the Allocator.
func (a *Allocator) Close() {
	close(a.closeC)
	<-a.doneC
}

// Run the Allocator.
func (a *Allocator) Run(
	info *metainfo.Info,
	sto storage.Storage,
	progressC chan Progress,
	resultC chan *Allocator,
	sybil *sybil.SybilInfo,
) {
	defer close(a.doneC)

	defer func() {
		if a.Error != nil {
			for _, f := range a.Files {
				if f.Storage != nil {
					f.Storage.Close()
				}
			}
		}
		select {
		case resultC <- a:
		case <-a.closeC:
		}
	}()

	var allocatedSize int64
	a.Files = make([]File, len(info.Files))
	for i, f := range info.Files {
		var sf storage.File
		var exists bool
		if f.Padding {
			sf = storage.NewPaddingFile(f.Length)
			a.Files[i] = File{Storage: sf, Name: f.Path, Padding: f.Padding}
		} else {
			// Don't care padding files for now
			// https://www.bittorrent.org/beps/bep_0047.html

			// k := int64(math.Ceil(float64(info.NumPieces)/float64(numIdentity)))
			// begin := info.PieceLength * k * identity
			// end   := info.PieceLength * k * (identity + 1)
			// var length int64
			// if begin >= f.Length {
				// panic("oops bad identity")
			// }
			// if end > f.Length {
				// length = f.Length - begin
			// } else {
				// length = end - begin
			// }

			path := fmt.Sprintf("%s.%d", f.Path, sybil.Identity)
			fmt.Printf("open file length: %dK\n", sybil.Length/1024)
			sf, exists, a.Error = sto.Open(path, sybil.Length)

			// sf, exists, a.Error = sto.Open(f.Path, f.Length)
			if a.Error != nil {
				return
			}
			if exists {
				a.HasExisting = true
			} else {
				a.HasMissing = true
			}
			a.Files[i] = File{Storage: sf, Name: path, Padding: f.Padding}
		}
		// a.Files[i] = File{Storage: sf, Name: f.Path, Padding: f.Padding}
		// a.Files[i] = File{Storage: sf, Name: path, Padding: f.Padding}
		// allocatedSize += f.Length
		allocatedSize += sybil.Length
		a.sendProgress(progressC, allocatedSize)
	}
}

func (a *Allocator) sendProgress(progressC chan Progress, size int64) {
	select {
	case progressC <- Progress{AllocatedSize: size}:
	case <-a.closeC:
		return
	}
}
