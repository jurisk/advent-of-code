import Common.Direction

structure Coords2D where
  x: Int
  y: Int
deriving BEq

instance [HAdd α β γ] : HAdd Coords2D Coords2D Coords2D where
  hAdd a b := { x := a.x + b.x, y := a.y + b.y}

def Coords2D.origin: Coords2D := { x := 0, y := 0 }
