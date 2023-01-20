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
