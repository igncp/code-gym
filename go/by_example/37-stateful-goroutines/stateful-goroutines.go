package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"sync/atomic"
	"time"
)

type readOp struct {
	key  int
	resp chan int
}

type writeOp struct {
	key  int
	val  int
	resp chan bool
}

var (
	cores           int
	ops             int64
	readGoroutines  int
	reads           chan *readOp
	state           map[int]int
	writeGoroutines int
	writes          chan *writeOp
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())

	reads = make(chan *readOp)
	writes = make(chan *writeOp)
	ops = 0
	cores = runtime.NumCPU()

	state = make(map[int]int)
	writeGoroutines = rand.Intn(1000)
	readGoroutines = rand.Intn(1000)
}

func main() {
	fmt.Printf("Running %v cores, %v read goroutines and %v write goroutines\n", cores, readGoroutines, writeGoroutines)

	go handleState()
	for r := 0; r < readGoroutines; r++ {
		go runReadOps()
	}
	for w := 0; w < writeGoroutines; w++ {
		go runWriteOps()
	}

	time.Sleep(time.Second)
	opsFinal := atomic.LoadInt64(&ops)
	fmt.Println("ops:", opsFinal)

	// The state was made public to be able to print it
	fmt.Println(state)
}

func handleState() {
	for {
		select {
		case read := <-reads:
			read.resp <- state[read.key]
		case write := <-writes:
			state[write.key] = write.val
			write.resp <- true
		}
	}
}

func runReadOps() {
	for {
		read := &readOp{
			key:  rand.Intn(5),
			resp: make(chan int),
		}

		reads <- read
		<-read.resp

		atomic.AddInt64(&ops, 1)
	}
}

func runWriteOps() {
	for {
		write := &writeOp{
			key:  rand.Intn(5),
			val:  rand.Intn(100),
			resp: make(chan bool),
		}

		writes <- write
		<-write.resp

		atomic.AddInt64(&ops, 1)
	}
}
