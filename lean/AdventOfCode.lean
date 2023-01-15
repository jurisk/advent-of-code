def splitLines (input: String): List String :=
  input.splitOn "\n"

def readFile (fileName: String): IO String := 
  IO.FS.readFile fileName

def readFileLines (fileName: String): IO (List String) := do
  let data <- readFile fileName
  pure (splitLines data)

def charToNat (ch: Char): Nat :=
  ch.toNat - '0'.toNat

def parseNat' (s: List Char) (acc: Nat): Option Nat :=
  match s with
    | [] => acc
    | h :: t =>
      if h.isDigit then
        parseNat' t (acc * 10 + charToNat h)
      else
        none

def parseNat (s: String): Option Nat :=
  parseNat' (String.toList s) 0

def parseNatUnsafe (s: String): Nat :=
  match parseNat s with
    | some x => x
    | none   => panic! "Failed to parse " ++ s

def Option.toList (x: Option α): List α :=
  match x with
    | some x => [x]
    | none => []

def List.sum [Add α] [OfNat α 0] (xs: List α): α := xs.foldl (· + ·) 0

def List.singleResultUnsafe [Inhabited α] [ToString α] (row: List α): α :=
  match row with
    | [single] => single
    | _ => panic!(s!"Expected a single result in {row}")

def List.flatten (list: List (List α)): List α :=
  match list with
    | [] => []
    | h :: t => h ++ t.flatten

def List.flattenOptions (list: List (Option α)): List α :=
  (List.map Option.toList list).flatten

def List.extractIfAllEqual [Inhabited α] [BEq α] (list: List α): α :=
  let result: α := list.head!
  assert! List.all list (· == result)
  result

structure Pair (α β : Type) : Type where
  first : α
  second : β

def Pair.both (a: α) (b: α): List (Pair α α) := 
  [
    { first := a, second := b },
    { first := b, second := a }
  ]

def List.allPairs (row: List α): List (Pair α α) :=
  let results: List (List (List (Pair α α))) := (List.map (fun a => 
    List.map (fun b => Pair.both a b) row
  ) row)

  results.flatten.flatten
