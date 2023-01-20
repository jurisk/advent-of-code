def splitLines (input: String): List String :=
  input.splitOn "\n"

def readFile (fileName: String): IO String := 
  IO.FS.readFile fileName

def readFileLines (fileName: String): IO (List String) := do
  let data <- readFile fileName
  pure (splitLines data)