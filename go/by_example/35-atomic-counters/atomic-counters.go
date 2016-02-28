package main

import (
	"fmt"
	"math/rand"
	"runtime"
	"sync/atomic"
	"time"
)

var (
	goroutines int
	cores      int
)

func loopIncreasingAtomicCounter(ops *uint64) {
	for {
		atomic.AddUint64(ops, 1)
		runtime.Gosched()
	}
}

func init() {
	rand.Seed(time.Now().UTC().UnixNano())

	cores = runtime.NumCPU()
	goroutines = rand.Intn(1000)
}

func main() {
	runtime.GOMAXPROCS(cores)

	fmt.Printf("Running %v cores and %v goroutines\n", cores, goroutines)

	var ops uint64 = 0

	for i := 0; i < goroutines; i++ {
		go loopIncreasingAtomicCounter(&ops)
	}

	time.Sleep(time.Second)
	opsFinal := atomic.LoadUint64(&ops)
	fmt.Println("ops:", opsFinal)
}
