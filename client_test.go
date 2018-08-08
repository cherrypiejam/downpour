package rain_test

import (
	"io"
	"io/ioutil"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"testing"
	"time"

	"github.com/Sirupsen/logrus"
	"github.com/cenkalti/log"
	"github.com/crosbymichael/tracker/registry/inmem"
	"github.com/crosbymichael/tracker/server"

	"github.com/cenkalti/rain"
	"github.com/cenkalti/rain/logger"
)

var (
	trackerAddr    = ":5000"
	torrentFile    = filepath.Join("testfiles", "10mb.torrent")
	torrentDataDir = "testfiles"
	torrentName    = "10mb"
)

func init() {
	logger.SetLogLevel(log.DEBUG)
}

func TestDownload(t *testing.T) {
	c1 := rain.NewConfig()
	c1.Port = 0 // pick a random port
	seeder, err := rain.NewClient(c1)
	if err != nil {
		t.Fatal(err)
	}

	err = seeder.Listen()
	if err != nil {
		t.Fatal(err)
	}

	where, err := ioutil.TempDir("", "rain-")
	if err != nil {
		t.Fatal(err)
	}

	leecher, err := rain.NewClient(nil)
	if err != nil {
		t.Fatal(err)
	}

	startTracker(t)

	f, err := os.Open(torrentFile)
	if err != nil {
		t.Fatal(err)
	}

	t1, err := seeder.AddTorrent(f, torrentDataDir)
	if err != nil {
		t.Fatal(err)
	}
	t1.Start()

	// Wait for seeder to announce to tracker.
	time.Sleep(100 * time.Millisecond)

	f.Seek(0, io.SeekStart)
	t2, err := leecher.AddTorrent(f, where)
	if err != nil {
		t.Fatal(err)
	}
	t2.Start()

	select {
	case <-t2.CompleteNotify():
	case <-time.After(4 * time.Second):
		panic("download did not finish")
	}

	cmd := exec.Command("diff", "-rq",
		filepath.Join(torrentDataDir, torrentName),
		filepath.Join(where, torrentName))
	err = cmd.Run()
	if err != nil {
		t.Fatal(err)
	}

	err = os.RemoveAll(where)
	if err != nil {
		t.Fatal(err)
	}
}

func startTracker(t *testing.T) {
	logger := logrus.New()
	logger.Level = logrus.DebugLevel
	registry := inmem.New()
	s := server.New(120, 30, registry, logger)
	l, err := net.Listen("tcp", trackerAddr)
	if err != nil {
		t.Fatal(err)
	}
	go http.Serve(l, s)
}
