import Foundation

let testFile = "Sources/2017/Day25/input/test.txt"
let testData = try! String(contentsOfFile: testFile)

let testResult = solve(input: testData)
print("Test result: \(testResult)")
assert(testResult == 3)

let realFile = "Sources/2017/Day25/input/real.txt"
let realData = try! String(contentsOfFile: realFile)

let realResult = solve(input: realData)
print("Real result: \(realResult)")
assert(realResult == 3_745)
