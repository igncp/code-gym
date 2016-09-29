package main

import (
	"fmt"
	"math"
)

type geometry interface {
	area() float64
	perim() float64
}
type rect struct {
	width, height float64
}
type circle struct {
	radius float64
}
type triangle struct {
	sides [2]float64
	angle float64
}

func (r rect) area() float64 {
	return r.width * r.height
}
func (r rect) perim() float64 {
	return 2*r.width + 2*r.height
}

func (c circle) area() float64 {
	return math.Pi * c.radius * c.radius
}
func (c circle) perim() float64 {
	return 2 * math.Pi * c.radius
}

func (t triangle) getThirdSide() float64 {
	a := t.sides[0]
	b := t.sides[1]
	c2 := math.Pow(a, 2) + math.Pow(b, 2) - 2*a*b*math.Cos(t.angle)
	c := math.Sqrt(c2)
	return c
}
func (t triangle) area() float64 {
	// http://www.mathopenref.com/heronsformula.html
	halfPerimeter := t.perim() / 2
	a := t.sides[0]
	b := t.sides[1]
	c := t.getThirdSide()
	a2 := halfPerimeter * (halfPerimeter - a) * (halfPerimeter - b) * (halfPerimeter - c)
	return math.Sqrt(a2)
}
func (t triangle) perim() float64 {
	return t.sides[0] + t.sides[1] + t.getThirdSide()
}

func measure(geometry string, g geometry) {
	fmt.Println(geometry+":", g)
	fmt.Println(geometry+" area:", g.area())
	fmt.Println(geometry+" perim:", g.perim())
	fmt.Println("")
}

func main() {
	r := rect{width: 3, height: 4}
	c := circle{radius: 5}
	t := triangle{sides: [2]float64{1, 2}, angle: 1}
	measure("rectangle", r)
	measure("circle", c)
	measure("triangle", t)
}
