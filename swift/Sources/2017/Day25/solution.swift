import Foundation

func part1(input: String) -> Int {
    input.count
}

let filename = "test.txt"

let contents = try! String(contentsOfFile: filename)

let lines = contents.split(separator:"\n")

print(part1(input: contents))
