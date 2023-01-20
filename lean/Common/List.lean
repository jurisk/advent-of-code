import Common.Option

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
