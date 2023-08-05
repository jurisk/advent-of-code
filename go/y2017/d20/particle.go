package main

import (
	"github.com/samber/lo"
)

type Particle struct {
	position     Vec3
	velocity     Vec3
	acceleration Vec3
}

func (p Particle) next() Particle {
	return p.afterT(1)
}

// p0, v0, a
//
// v1 = v0 + a
// p1 = p0 + v1 = p0 + (v0 + a)
//
// v2 = v1 + a = v0 + 2*a
// p2 = p1 + v2 = (p0 + (v0 + a)) + (v0 + 2*a) = p0 + 2*v0 + 3*a
//
// v3 = v2 + a = v0 + 3*a
// p3 = p2 + v3 = (p0 + 2*v0 + 3*a) + (v0 + 3*a) = p0 + 3*v0 + 6*a

// v4 = v3 + a = v0 + 4*a
// p4 = p3 + v4 = (p0 + 3*v0 + 6*a) + (v0 + 4*a) = p0 + 4*v0 + 10*a
// p4 = (((p0 + (v0 + a)) + ((v0 + a) + a)) + (((v0 + a) + a) + a)) + ((((v0 + a) + a) + a) + a)
// p4 = (p0 + 3*v0 + (3 + 2 + 1)*a) + (v0 + 4*a) = p0 + 4*v0 + (((4 + 1) * 4) / 2) * a

func (p Particle) velocityAfterT(t int) Vec3 {
	return p.velocity.Add(p.acceleration.Mul(t))
}

func (p Particle) positionAfterT(t int) Vec3 {
	return p.position.Add(p.velocity.Mul(t)).Add(p.acceleration.Mul(((t + 1) * t) / 2))
}

// We never call this with anything else but `t == 1`, so really we could remove all these more complex
// calculations and leave just `next()` as described in the task description.
func (p Particle) afterT(t int) Particle {
	return Particle{
		position:     p.positionAfterT(t),
		velocity:     p.velocityAfterT(t),
		acceleration: p.acceleration,
	}
}

func afterT(particles []Particle, t int) []Particle {
	return lo.Map(particles, func(p Particle, index int) Particle { return p.afterT(t) })
}

func next(particles []Particle) []Particle {
	return afterT(particles, 1)
}
