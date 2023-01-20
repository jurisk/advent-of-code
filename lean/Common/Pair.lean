import Common.List

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
