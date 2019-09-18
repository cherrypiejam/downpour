package boltdbresumer

import (
	"encoding/base64"
	"encoding/json"
	"time"
)

type Spec struct {
	InfoHash        []byte
	Dest            string
	Port            int
	Name            string
	Trackers        [][]string
	URLList         []string
	FixedPeers      []string
	Info            []byte
	Bitfield        []byte
	AddedAt         time.Time
	BytesDownloaded int64
	BytesUploaded   int64
	BytesWasted     int64
	SeededFor       time.Duration
	Started         bool
}

type jsonSpec struct {
	Dest            string
	Port            int
	Name            string
	Trackers        [][]string
	URLList         []string
	FixedPeers      []string
	AddedAt         time.Time
	BytesDownloaded int64
	BytesUploaded   int64
	BytesWasted     int64
	Started         bool

	// JSON safe types
	InfoHash  string
	Info      string
	Bitfield  string
	SeededFor int64
}

func (s Spec) MarshalJSON() ([]byte, error) {
	j := jsonSpec{
		Dest:            s.Dest,
		Port:            s.Port,
		Name:            s.Name,
		Trackers:        s.Trackers,
		URLList:         s.URLList,
		FixedPeers:      s.FixedPeers,
		AddedAt:         s.AddedAt,
		BytesDownloaded: s.BytesDownloaded,
		BytesUploaded:   s.BytesUploaded,
		BytesWasted:     s.BytesWasted,
		Started:         s.Started,

		InfoHash:  base64.StdEncoding.EncodeToString(s.InfoHash),
		Info:      base64.StdEncoding.EncodeToString(s.Info),
		Bitfield:  base64.StdEncoding.EncodeToString(s.Bitfield),
		SeededFor: int64(s.SeededFor),
	}
	return json.Marshal(j)
}

func (s *Spec) UnmarshalJSON(b []byte) error {
	var j jsonSpec
	err := json.Unmarshal(b, &j)
	if err != nil {
		return err
	}
	s.InfoHash, err = base64.StdEncoding.DecodeString(j.InfoHash)
	if err != nil {
		return err
	}
	s.Info, err = base64.StdEncoding.DecodeString(j.Info)
	if err != nil {
		return err
	}
	s.Bitfield, err = base64.StdEncoding.DecodeString(j.Bitfield)
	if err != nil {
		return err
	}
	s.SeededFor = time.Duration(j.SeededFor)
	s.Dest = j.Dest
	s.Port = j.Port
	s.Name = j.Name
	s.Trackers = j.Trackers
	s.URLList = j.URLList
	s.FixedPeers = j.FixedPeers
	s.AddedAt = j.AddedAt
	s.BytesDownloaded = j.BytesDownloaded
	s.BytesUploaded = j.BytesUploaded
	s.BytesWasted = j.BytesWasted
	s.Started = j.Started
	return nil
}
