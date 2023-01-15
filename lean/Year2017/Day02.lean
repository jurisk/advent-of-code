import «AdventOfCode»

def parseLine (line: String): List Nat :=
  List.map parseNatUnsafe (line.splitOn "\t")

def parseLines (lines: List String): List (List Nat) :=
  List.map parseLine lines

def rowChecksum (row: List Nat): Nat :=
  let min := (List.minimum? row).get!
  let max := (List.maximum? row).get!
  max - min

def pairResult (pair: Pair Nat Nat): Option Nat :=
  let a := pair.first
  let b := pair.second
  if a = b
    then none
    else (
      let ab: Option Nat := if a % b = 0 then some (a / b) else none
      let ba: Option Nat := if b % a = 0 then some (b / a) else none
      ab.orElse (fun ⟨⟩ => ba)
    )
  
def evenDivisions (row: List Nat): Nat :=
  let pairs: List (Pair Nat Nat) := row.allPairs
  let resultOptions: List (Option Nat) := List.map pairResult pairs 
  let results: List Nat := resultOptions.flattenOptions
  results.extractIfAllEqual

def solveTask (f: (List Nat) → Nat) (data: List (List Nat)): Nat :=
  (List.map f data).sum

def solve1 (data: List (List Nat)): Nat := 
  solveTask rowChecksum data

def solve2 (data: List (List Nat)): Nat := 
  solveTask evenDivisions data

def main : IO Unit := do
  let input <- readFileLines "Year2017/Day02.txt"
  let data := parseLines input

  let result1 := solve1 data
  IO.println s!"Part 1: {result1}"
  assert! result1 == 41887

  let result2 := solve2 data
  IO.println s!"Part 2: {result2}"
  assert! result2 == 226
