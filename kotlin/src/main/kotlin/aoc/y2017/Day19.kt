package aoc.y2017

enum class Direction {
    N, E, S, W;

    fun left() = when (this) {
        N -> W
        W -> S
        S -> E
        E -> N
    }

    fun right() = when (this) {
        N -> E
        E -> S
        S -> W
        W -> N
    }
}

sealed class Square {
    object Empty : Square()
    object Vertical : Square()
    object Horizontal : Square()
    object Corner : Square()
    data class Letter(val value: Char) : Square() {
        init {
            require(value.isLetter() && value.isUpperCase()) { "Letter must be an uppercase Latin letter." }
        }
    }
}

data class Coords2D(val x: Int, val y: Int) {
    fun moveInDirection(direction: Direction): Coords2D =
        when (direction) {
            Direction.N -> Coords2D(x, y - 1)
            Direction.E -> Coords2D(x + 1, y)
            Direction.S -> Coords2D(x, y + 1)
            Direction.W -> Coords2D(x - 1, y)
        }
}

data class Grid<T>(val squares: List<List<T>>) {
    fun getSquare(coords: Coords2D): T? =
        squares.getOrNull(coords.y)?.getOrNull(coords.x)

    fun findXAtY(y: Int, predicate: (T) -> Boolean): Int? =
        squares.getOrNull(y)?.indexOfFirst(predicate)
}

data class State(
    val position: Coords2D,
    val direction: Direction,
    val lettersVisited: List<Square.Letter>,
    val stepsTaken: Int
) {
    private fun getSquare(grid: Grid<Square>, position: Coords2D): Square =
        grid.getSquare(position) ?: Square.Empty

    fun next(grid: Grid<Square>): State? {
        val currentSquare = getSquare(grid, position)

        val directlyNextPosition = position.moveInDirection(direction)

        val result = when (currentSquare) {
            is Square.Empty -> null // We ran out of track, finishing
            is Square.Corner -> {
                val left = direction.left()
                val right = direction.right()
                val leftPosition = position.moveInDirection(left)
                val leftSquare = getSquare(grid, leftPosition)
                val rightPosition = position.moveInDirection(right)
                val rightSquare = getSquare(grid, rightPosition)
                when {
                    leftSquare !is Square.Empty -> copy(position = leftPosition, direction = left)
                    rightSquare !is Square.Empty -> copy(position = rightPosition, direction = right)
                    else -> null // Both sides are empty, we're at a dead end
                }
            }
            is Square.Horizontal, is Square.Vertical -> copy(position = directlyNextPosition)
            is Square.Letter -> copy(position = directlyNextPosition, lettersVisited = lettersVisited + currentSquare)
        }

        return result?.copy(stepsTaken = result.stepsTaken + 1)
    }

    fun representation(grid: Grid<Square>): String {
        return grid.squares.mapIndexed { y, row ->
            row.mapIndexed { x, square ->
                when {
                    position.x == x && position.y == y -> 'X'
                    square is Square.Empty -> ' '
                    square is Square.Vertical -> '|'
                    square is Square.Horizontal -> '-'
                    square is Square.Corner -> '+'
                    square is Square.Letter -> square.value
                    else -> '?'
                }
            }.joinToString("")
        }.joinToString("\n")
    }
}

private fun parse(input: String): Grid<Square> {
    val lines = input.lines()
    val squares = lines.map { line ->
        line.map { char ->
            when {
                char.isWhitespace() -> Square.Empty
                char == '|' -> Square.Vertical
                char == '-' -> Square.Horizontal
                char == '+' -> Square.Corner
                char.isLetter() && char.isUpperCase() -> Square.Letter(char)
                else -> throw IllegalArgumentException("Invalid character: $char")
            }
        }
    }

    return Grid(squares)
}

private fun solve1(grid: Grid<Square>): State {
    val start = Coords2D(grid.findXAtY(0) { it == Square.Vertical } ?: throw RuntimeException("Cannot find starting square"), 0)

    var currentState = State(
        position = start,
        direction = Direction.S,
        lettersVisited = emptyList(),
        stepsTaken = 0
    )

    while (true) {
        val nextState = currentState.next(grid)
        if (nextState == null) {
            break
        } else {
            currentState = nextState
        }
    }

    return currentState
}

fun part1(input: String): String =
    solve1(parse(input)).lettersVisited.map { it.value }.joinToString("")

fun part2(input: String): Int =
    solve1(parse(input)).stepsTaken
