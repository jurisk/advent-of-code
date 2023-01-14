module Day01 =
    let parse (input: string): int[] =
        let inline charToInt ch = int ch - int '0'
        input |> Seq.toArray |> Array.map charToInt

    let solve (numbers: int[], offset: int): int =
        let firstIfEqual a b= if a = b then a else 0
        let f idx = firstIfEqual numbers[idx] numbers[(idx + offset) % numbers.Length]
        List.sumBy f [0..numbers.Length - 1]

    let solve1 (numbers: int[]): int = 
        solve(numbers, 1)

    let solve2 (numbers: int[]): int = 
        assert (numbers.Length % 2 = 0)
        solve(numbers, numbers.Length / 2)        

    let part1 (input: string): int =
        input |> parse |> solve1

    let part2 (input: string): int =
        input |> parse |> solve2

open Day01
open Common.IO

[<EntryPoint>]
let main argv =
    let realLines: list<string> = readLines "Year2017/Day01.txt"

    let checkPart1 input expected =
        assert ((part1 input ) = expected)

    checkPart1 "1122" 3
    checkPart1 "1111" 4
    checkPart1 "1234" 0
    checkPart1 "91212129" 9
    
    let result1 = realLines.Head |> part1
    result1 |> printf "Part 1: %d\n"
    assert (result1 = 1171)

    let checkPart2 input expected =
        assert ((part2 input) = expected)

    checkPart2 "1212" 6
    checkPart2 "1221" 0
    checkPart2 "123425" 4
    checkPart2 "123123" 12
    checkPart2 "12131415" 4

    let result2 = realLines.Head |> part2
    result2 |> printf "Part 2: %d\n"
    assert (result2 = 1024)

    0
