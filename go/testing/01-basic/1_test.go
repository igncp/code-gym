package basic1

import (
	"testing"
)

func TestDifferent(t *testing.T) {
	a := 1
	b := 2

	if a == b {
		t.Errorf("Test failed, a == b (%v)", a)
	}
}

func TestEqual(t *testing.T) {
	a := 1
	b := 1

	if a != b {
		t.Errorf("Test failed, a != b (%v, %v)", a, b)
	}
}
