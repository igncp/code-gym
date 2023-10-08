package main

import (
	"fmt"
	"math/rand"
	"time"
)

func receiveJobs(jobs chan int, done chan bool) {
	for {
		j, more := <-jobs
		if more {
			fmt.Println("received job", j)
		} else {
			fmt.Println("received all jobs")
			done <- true
			return
		}
	}
}

func sendJobs(jobs chan int, jobsToSend int) {
	for j := 1; j <= jobsToSend; j++ {
		fmt.Println("sending job", j)
		jobs <- j
	}

	close(jobs)
	fmt.Println("sent all jobs")
}

func main() {
	rand.Seed(time.Now().UTC().UnixNano())

	jobsChannelSize := rand.Intn(10)
	jobsNumberToSend := rand.Intn(10)

	fmt.Printf("Jobs channel size: %v\n", jobsChannelSize)
	fmt.Printf("Jobs number to send: %v\n", jobsNumberToSend)
	fmt.Println("")

	jobs := make(chan int, jobsChannelSize)
	done := make(chan bool)

	go receiveJobs(jobs, done)

	go sendJobs(jobs, jobsNumberToSend)

	<-done
}
