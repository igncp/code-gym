package main

import "math"
import "fmt"

func round(v float64, decimals int) float64 {
  var pow float64 = 1

  for i:=0; i<decimals; i++ {
     pow *= 10
  }

  return float64(int((v * pow) + 0.5)) / pow
}

func main() {
  fmt.Println("go" + "lang")
  fmt.Println("math.Sqrt(2)", math.Sqrt(2))
  fmt.Println("round(math.Sqrt(2), 2)", round(math.Sqrt(2), 2))
  fmt.Println("7.0/3.0", 7.0/3.0)

  fmt.Println("true && false", true && false)
  fmt.Println("true || false", true || false)
  fmt.Println("!true", !true)
}
