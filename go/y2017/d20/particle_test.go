package main

import (
	"reflect"
	"testing"
)

var tests = []struct {
	name     string
	input    int
	expected []Particle
}{
	{
		"t1",
		1,

		[]Particle{
			{position: Vec3{x: 4, y: 0, z: 0}, velocity: Vec3{x: 1, y: 0, z: 0}, acceleration: Vec3{x: -1, y: 0, z: 0}},
			{position: Vec3{x: 2, y: 0, z: 0}, velocity: Vec3{x: -2, y: 0, z: 0}, acceleration: Vec3{x: -2, y: 0, z: 0}},
		},
	},
	{
		"t2",
		2,
		[]Particle{
			{position: Vec3{x: 4, y: 0, z: 0}, velocity: Vec3{x: 0, y: 0, z: 0}, acceleration: Vec3{x: -1, y: 0, z: 0}},
			{position: Vec3{x: -2, y: 0, z: 0}, velocity: Vec3{x: -4, y: 0, z: 0}, acceleration: Vec3{x: -2, y: 0, z: 0}},
		},
	},
	{
		"t3",
		3, []Particle{
			{position: Vec3{x: 3, y: 0, z: 0}, velocity: Vec3{x: -1, y: 0, z: 0}, acceleration: Vec3{x: -1, y: 0, z: 0}},
			{position: Vec3{x: -8, y: 0, z: 0}, velocity: Vec3{x: -6, y: 0, z: 0}, acceleration: Vec3{x: -2, y: 0, z: 0}},
		},
	},
}

func TestAfterT(t *testing.T) {
	var particles = []Particle{
		{position: Vec3{x: 3, y: 0, z: 0}, velocity: Vec3{x: 2, y: 0, z: 0}, acceleration: Vec3{x: -1, y: 0, z: 0}},
		{position: Vec3{x: 4, y: 0, z: 0}, velocity: Vec3{x: 0, y: 0, z: 0}, acceleration: Vec3{x: -2, y: 0, z: 0}},
	}

	for _, test := range tests {
		t.Run(test.name, func(*testing.T) {
			obtained := afterT(particles, test.input)
			if !reflect.DeepEqual(obtained, test.expected) {
				t.Errorf("expected %v, obtained %v", test.expected, obtained)
			}
		})
	}
}
