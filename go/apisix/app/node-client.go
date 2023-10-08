package app

import (
	"bytes"
	"encoding/json"
	"io/ioutil"
	"log"
	"net/http"
)

type NodeClient struct{}

func NewNodeClient() *NodeClient {
	return &NodeClient{}
}

func (n *NodeClient) Request() string {
	req, err := http.NewRequest("GET", "http://localhost:9080/", nil)

	if err != nil {
		log.Fatal(err)
	}

	client := &http.Client{}
	resp, err := client.Do(req)

	if err != nil {
		log.Fatal(err)
	}

	responseBody, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		log.Fatal(err)
	}

	var out bytes.Buffer
	json.Indent(&out, responseBody, "", "  ")

	return string(out.Bytes())
}
