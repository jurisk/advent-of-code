package main

import (
	"fmt"
	"log"
	"os"
	"strings"
)

func parseFile(fileName string) (particles []Particle) {
	bytes, err := os.ReadFile(fileName)

	if err != nil {
		log.Fatal(err)
	}

	input := string(bytes)

	for _, line := range strings.Split(input, "\n") {
		p := Particle{}
		_, err := fmt.Sscanf(line, "p=<%d,%d,%d>, v=<%d,%d,%d>, a=<%d,%d,%d>",
			&p.position.x,
			&p.position.y,
			&p.position.z,
			&p.velocity.x,
			&p.velocity.y,
			&p.velocity.z,
			&p.acceleration.x,
			&p.acceleration.y,
			&p.acceleration.z,
		)
		if err != nil {
			log.Fatal(err)
		}
		particles = append(particles, p)
	}

	return particles
}
