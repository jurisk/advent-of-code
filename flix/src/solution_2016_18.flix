type alias ErrorMessage = String

enum Tile with Eq {
    case Safe,
    case Trap
}

type alias Row = Vector[Tile]
type alias Field = Vector[Row]

mod Tile {
    pub def parse(ch: Char): Result[ErrorMessage, Tile] =
        match ch {
            case '.' => Ok(Tile.Safe)
            case '^' => Ok(Tile.Trap)
            case _   => Err("Unrecognized character ${ch}")
        }

    pub def next(left: Tile, center: Tile, right: Tile): Tile =
        match (left, center, right) {
            case (Tile.Trap, Tile.Trap, Tile.Safe)  => Tile.Trap
            case (Tile.Safe, Tile.Trap, Tile.Trap)  => Tile.Trap
            case (Tile.Trap, Tile.Safe, Tile.Safe)  => Tile.Trap
            case (Tile.Safe, Tile.Safe, Tile.Trap)  => Tile.Trap
            case _                                  => Tile.Safe
        }
}

mod Row {
    pub def parse(s: String): Result[ErrorMessage, Row] =
        s |> String.toList |> List.toVector |> Vector.traverse(Tile.parse)

    pub def next(row: Row): Row =
        row |> Vector.mapWithIndex( idx -> center ->
            let left    = row |> Vector.nth(idx - 1) |> Option.getWithDefault(Tile.Safe);
            let right   = row |> Vector.nth(idx + 1) |> Option.getWithDefault(Tile.Safe);
            Tile.next(left, center, right)
        )
}

mod Field {
    pub def build(firstRow: Row, rows: Int32): Field =
        region f {
            let arr = Array.new(f, rows);
            Array.put(firstRow, 0, arr);
            List.range(1, rows) |> List.forEach(idx -> {
                let previousRow = arr |> Array.get(idx - 1);
                let generatedRow = previousRow |> Row.next;
                arr |> Array.put(generatedRow, idx)
            });
            Array.toVector(arr)
        }

    pub def countSafe(field: Field): Int32 = 
        field |> (Tile.Safe |> Eq.eq |> Vector.count |> Vector.sumWith)
}

def solve1(input: String, rows: Int32): Result[ErrorMessage, Int32] =
    forM (
        firstRow <- Row.parse(input)
    ) yield {
        Field.build(firstRow, rows) |> Field.countSafe
    }

def data(): String =
    ".^^^^^.^^.^^^.^...^..^^.^.^..^^^^^^^^^^..^...^^.^..^^^^..^^^^...^.^.^^^^^^^^....^..^^^^^^.^^^.^^^.^^"

def main(): Unit \ IO = {
    let result1 = solve1(data(), 40);
    println("Part 1: ${result1}");

    let result2 = solve1(data(), 400_000);
    println("Part 2: ${result2}");

    ()
}

@Test 
def part1_test1(): Bool =
    Assert.eq(Ok(6), solve1("..^^.", 3))

@Test 
def part1_test2(): Bool =
    Assert.eq(Ok(38), solve1(".^^.^.^^^^", 10))

@Test 
def part1_real(): Bool =
    Assert.eq(Ok(1989), solve1(data(), 40))

@Test 
def part2_real(): Bool =
    Assert.eq(Ok(19_999_894), solve1(data(), 400_000))