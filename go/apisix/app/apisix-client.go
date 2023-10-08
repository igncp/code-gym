package app

import (
	"bytes"
	"encoding/json"
	"io"
	"io/ioutil"
	"log"
	"net/http"
)

type ApisixClient struct {
	key string
}

func NewApisixClient() *ApisixClient {
	return &ApisixClient{
		key: "edd1c9f034335f136f87ad84b625c8f1",
	}
}

func (a *ApisixClient) query(url string, method string, body io.Reader) string {
	req, err := http.NewRequest(method, url, body)

	if err != nil {
		log.Fatal(err)
	}

	req.Header.Add("x-api-key", a.key)
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

func (a *ApisixClient) GetRoutes() string {
	return a.query("http://localhost:9180/apisix/admin/routes", "GET", nil)
}

func (a *ApisixClient) AddRoute() string {
	return a.query("http://localhost:9180/apisix/admin/routes", "PUT",
		bytes.NewBuffer([]byte(
			`{
	"id": "1",
	"uri": "/",
	"upstream": {
		"type": "roundrobin",
		"nodes": {
			"web1:80": 1
		}
	}
}`)))
}
