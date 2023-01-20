inductive Direction 
  | North 
  | East 
  | South 
  | West
  deriving Inhabited, BEq

def Direction.toString (d: Direction): String :=
  match d with 
      | North => "N"
      | East => "E"
      | South => "S"
      | West => "W"

instance: ToString Direction := ⟨fun d => d.toString⟩

def Direction.parseUnsafeNESW (input: Char): Direction :=
  match input with
    | 'N' => North
    | 'E' => East
    | 'S' => South
    | 'W' => West
    | _   => panic! s!"Failed to parse `{input}`"

def Direction.parseUnsafeURDL (input: Char): Direction :=
  match input with
    | 'U' => North
    | 'R' => East
    | 'D' => South
    | 'L' => West
    | _   => panic! s!"Failed to parse `{input}`"
