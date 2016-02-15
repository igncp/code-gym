package main

import (
	log "github.com/Sirupsen/logrus"
)

func l(str string) {
	log.Info(str)
}

func main() {
	messages := make(chan string)
	signals := make(chan bool)
	select {
	case msg := <-messages:
		l("received message: " + msg)
	default:
		l("no message received")
	}
	msg := "hi"
	select {
	case messages <- msg:
		l("sent message" + msg)
	default:
		l("no message sent")
	}
	select {
	case msg := <-messages:
		l("received message" + msg)
	case <-signals:
		l("received signal")
	default:
		l("no activity")
	}
}
