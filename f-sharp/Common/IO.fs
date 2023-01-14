namespace Common

module IO =
    let readLines (filePath: string) = List.ofSeq(System.IO.File.ReadLines(filePath))
