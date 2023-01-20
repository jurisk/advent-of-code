def Option.toList (x: Option α): List α :=
  match x with
    | some x => [x]
    | none => []
