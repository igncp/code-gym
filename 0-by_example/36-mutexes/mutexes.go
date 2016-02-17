package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"sync"
	"sync/atomic"
	"time"
)

var (
	cores           int
	mutex           *sync.Mutex
	ops             int64
	readGoroutines  int
	state           map[int]int
	writeGoroutines int
)

func init() {
	rand.Seed(time.Now().UTC().UnixNano())

	cores = runtime.NumCPU()
	readGoroutines = rand.Intn(1000)
	writeGoroutines = rand.Intn(1000)

	state = make(map[int]int)
	mutex = &sync.Mutex{}
	ops = 0
}

func main() {
	runtime.GOMAXPROCS(cores)
	fmt.Printf("Running %v cores, %v read goroutines and %v write goroutines\n", cores, readGoroutines, writeGoroutines)

	for r := 0; r < readGoroutines; r++ {
		go readState()
	}
	for w := 0; w < writeGoroutines; w++ {
		go writeState()
	}

	time.Sleep(time.Second)

	opsFinal := atomic.LoadInt64(&ops)
	fmt.Println("ops:", opsFinal)
	mutex.Lock()
	fmt.Println("state:", state)
	mutex.Unlock()
}

func readState() {
	total := 0
	for {
		key := rand.Intn(5)
		mutex.Lock()
		total += state[key]
		mutex.Unlock()

		atomic.AddInt64(&ops, 1)
		runtime.Gosched()
	}
}

func writeState() {
	for {
		key := rand.Intn(5)
		val := rand.Intn(100)
		mutex.Lock()
		state[key] = val
		mutex.Unlock()
		atomic.AddInt64(&ops, 1)
		runtime.Gosched()
	}
}
