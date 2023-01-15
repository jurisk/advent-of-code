import Lake
open Lake DSL

package «advent-of-code» {
  isLeanOnly := true
}

require mathlib from git
  "https://github.com/leanprover-community/mathlib4.git"

lean_lib «AdventOfCode» {
  -- add library configuration options here
}

@[default_target]
lean_exe «year-2017-day-02» {
  root := `Year2017.Day02
}