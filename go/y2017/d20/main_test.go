package main

import "testing"

var testsPart1 = []struct {
	name     string
	input    string
	expected int
}{
	{"test", "test-1.txt", 0},
	{"real", "real.txt", 119},
}

func TestPart1(t *testing.T) {
	for _, test := range testsPart1 {
		t.Run(test.name, func(*testing.T) {
			obtained := Part1(test.input)
			if obtained != test.expected {
				t.Errorf("expected %v, obtained %v", test.expected, obtained)
			}
		})
	}
}

var testsPart2 = []struct {
	name     string
	input    string
	expected int
}{
	{"test", "test-2.txt", 1},
	{"real", "real.txt", 471},
}

func TestPart2(t *testing.T) {
	for _, test := range testsPart2 {
		t.Run(test.name, func(*testing.T) {
			obtained := Part2(test.input)
			if obtained != test.expected {
				t.Errorf("expected %v, obtained %v", test.expected, obtained)
			}
		})
	}
}
