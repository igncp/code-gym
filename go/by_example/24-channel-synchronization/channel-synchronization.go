package main

import (
	"fmt"
	"time"
)

type Worker struct {
	name string
}

func (w Worker) work(mainC chan bool) {
	fmt.Printf("%v working...\n", w.name)
	time.Sleep(time.Second)
	fmt.Printf("%v is done\n", w.name)

	mainC <- true
}

func main() {
	mainChannel := make(chan bool, 3)

	w1 := Worker{"foo"}
	w2 := Worker{"bar"}
	w3 := Worker{"baz"}

	go w1.work(mainChannel)
	go w2.work(mainChannel)
	go w3.work(mainChannel)

	<-mainChannel

	fmt.Println("All work 's dne!")
}
