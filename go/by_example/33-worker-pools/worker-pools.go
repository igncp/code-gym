package main

import (
	"github.com/fatih/color"
	"math/rand"
	"time"
)

func cyan(s ...interface{}) {
	color.New(color.FgCyan).Add(color.BlinkSlow).PrintlnFunc()(s...)
}

func magenta(s ...interface{}) {
	color.New(color.FgMagenta).Add(color.BlinkSlow).PrintlnFunc()(s...)
}

func green(s ...interface{}) {
	color.New(color.FgGreen).Add(color.BlinkSlow).PrintlnFunc()(s...)
}

func worker(id int, jobs <-chan int, results chan<- int) {
	for j := range jobs {
		var fn func(...interface{})
		switch id {
		case 1:
			fn = cyan
		case 2:
			fn = magenta
		case 3:
			fn = green
		}
		μs := rand.Intn(1000)
		fn("worker", id, "processing job", j, "in", μs, "μs")
		time.Sleep(time.Duration(μs) * time.Microsecond)
		results <- j * 2
	}
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())
	jobsNumber := 100

	jobs := make(chan int, 100)
	results := make(chan int, 100)

	for w := 1; w <= 3; w++ {
		go worker(w, jobs, results)
	}

	for j := 1; j <= jobsNumber; j++ {
		jobs <- j
	}

	close(jobs)

	for a := 1; a <= jobsNumber; a++ {
		<-results
	}
}
