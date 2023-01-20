import «Common»

structure Directions where
  directions: List Direction
deriving Inhabited

def parseLine (line: String): Directions :=
  let directions := List.map Direction.parseUnsafeURDL line.toList
  { directions := directions }

def parseLines (lines: List String): List Directions :=
  List.map parseLine lines

def moveClamped (f: Coords2D -> Option Char) (c: Coords2D) (d: Direction): Coords2D :=
  let moved := c.move d
  match f moved with
    | none => c
    | some _ => moved

def moveManyCapped (directions: Directions) (start: Coords2D) (f: Coords2D -> Option Char): Coords2D :=
  List.foldl (moveClamped f) start directions.directions

def solveWithF' (f: Coords2D -> Option Char) (directions: List Directions) (curr: Coords2D): List Char :=
  match directions with
    | [] => []
    | h :: t => 
      let next := moveManyCapped h curr f
      let nextValue := (f next).get!
      nextValue :: (solveWithF' f t next)

def solveWithF (f: Coords2D -> Option Char) (directions: List Directions): String :=
  let chars := solveWithF' f directions Coords2D.origin
  String.mk chars

def coordsToChar1 (c: Coords2D): Option Char :=
  match c with
    | { x := -1, y := -1 } => some '1'
    | { x := 0, y := -1 } => some '2'
    | { x := 1, y := -1 } => some '3'
    | { x := -1, y := 0 } => some '4'
    | { x := 0, y := 0 } => some '5'
    | { x := 1, y := 0 } => some '6'
    | { x := -1, y := 1 } => some '7'
    | { x := 0, y := 1 } => some '8'
    | { x := 1, y := 1 } => some '9'
    | _ => none

def solve1 (directions: List Directions): String := 
  solveWithF coordsToChar1 directions

def coordsToChar2 (c: Coords2D): Option Char :=
  match c with
    | { x := 2, y := -2 } => some '1'
    | { x := 1, y := -1 } => some '2'
    | { x := 2, y := -1 } => some '3'
    | { x := 3, y := -1 } => some '4'
    | { x := 0, y := 0 } => some '5'
    | { x := 1, y := 0 } => some '6'
    | { x := 2, y := 0 } => some '7'
    | { x := 3, y := 0 } => some '8'
    | { x := 4, y := 0 } => some '9'
    | { x := 1, y := 1 } => some 'A'
    | { x := 2, y := 1 } => some 'B'
    | { x := 3, y := 1 } => some 'C'
    | { x := 2, y := 2 } => some 'D'
    | _ => none
    
def solve2 (directions: List Directions): String := 
  solveWithF coordsToChar2 directions 

def main : IO Unit := do
  let realInput <- readFileLines "Year2016/Day02.txt"
  let realData := parseLines realInput

  let testInput <- readFileLines "Year2016/Day02-test.txt"
  let testData := parseLines testInput

  let testResult1 := solve1 testData
  assert! testResult1 == "1985"

  let result1 := solve1 realData
  IO.println s!"Part 1: {result1}"
  assert! result1 == "99332"

  let testResult2 := solve2 testData
  assert! testResult2 == "5DB3"

  let result2 := solve2 realData
  IO.println s!"Part 2: {result2}"
  assert! result2 == "DD483"
