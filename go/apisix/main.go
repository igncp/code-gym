package main

import (
	"apisix/app"
)

func main() {
	apisixClient := app.NewApisixClient()
	nodeClient := app.NewNodeClient()

	println("Routes:")
	println(apisixClient.GetRoutes())

	println("Adding route")
	apisixClient.AddRoute()

	println("Routes:")
	println(apisixClient.GetRoutes())

	println("Node response:")
	println(nodeClient.Request())
}
