package main

type Vec3 struct {
	x int
	y int
	z int
}

func (v Vec3) Mul(n int) Vec3 {
	return Vec3{
		x: v.x * n,
		y: v.y * n,
		z: v.z * n,
	}
}

func (v Vec3) Add(n Vec3) Vec3 {
	return Vec3{
		x: v.x + n.x,
		y: v.y + n.y,
		z: v.z + n.z,
	}
}

func (v Vec3) ManhattanDistanceIsLarger(n Vec3) bool {
	return v.ManhattanDistance() > n.ManhattanDistance()
}

func abs(x int) int {
	if x < 0 {
		return -x
	} else {
		return x
	}
}
func (v Vec3) ManhattanDistance() int {
	return abs(v.x) + abs(v.y) + abs(v.z)
}
