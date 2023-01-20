import Common.Coords2D

def Direction.diff (d: Direction): Coords2D :=
  match d with 
        | North => { x:= 0, y:= -1 }
        | East => { x:= 1, y := 0 }
        | South => { x:= 0, y:= 1 }
        | West => { x:= -1, y:= 0 }
        
def Coords2D.move (c: Coords2D) (d: Direction): Coords2D :=
  c + (Direction.diff d)
