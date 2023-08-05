package main

import (
	"github.com/samber/lo"
)

func Solve1(initial []Particle) int {
	result, _ := MinIndexByKey(initial, func(p Particle) int {
		// In case there are identical accelerations, we should consider velocities, and positions,
		// but our test data did not have such a case.
		//
		// Thus sorting would be better here to handle the general case, but we could avoid it, as it
		// requires too much ceremony in Go.
		return p.acceleration.ManhattanDistance()
	})

	return result
}

func Solve2(initial []Particle) int {
	particles := initial

	// Larger thresholds may be needed for other test data
	const ArbitraryThreshold = 1000

	for i := 0; i < ArbitraryThreshold; i++ {
		positions := lo.Map(particles, func(p Particle, idx int) Vec3 {
			return p.position
		})

		positionCounts := lo.CountValues(positions)

		// We could have used some set instead, but it seems there is no built-in one
		var conflictingPoints []Vec3

		// Apparently, no easy way to convert a Map to iterable, so we cannot use Filter
		for k, v := range positionCounts {
			if v > 1 {
				conflictingPoints = append(conflictingPoints, k)
			}
		}

		particles = lo.Reject(particles, func(p Particle, idx int) bool {
			return lo.Contains(conflictingPoints, p.position)
		})

		particles = next(particles)
	}

	return len(particles)
}
