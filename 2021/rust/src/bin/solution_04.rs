use nalgebra::{Matrix1x5, Matrix5, SMatrix};

type Number = u8;
type BoardIndex = usize;
type Error = String;

#[derive(Debug)]
struct Board {
    data: Matrix5<Number>,
}

impl Board {
    fn parse(data: &str) -> Result<Board, Error> {
        let tmp_vec: Result<Vec<Matrix1x5<Number>>, Error> = data
            .split('\n')
            .filter(|row| !row.is_empty())
            .map(|row| {
                let v: Result<Vec<Number>, Error> = row
                    .split_whitespace()
                    .map(|x| x.parse().map_err(|e| format!("{e} parsing {x}")))
                    .collect();

                v.map(Matrix1x5::from_vec)
            })
            .collect();

        let vec = tmp_vec?;
        let data: Matrix5<Number> = SMatrix::from_rows(&vec);

        Ok(Board { data })
    }

    fn is_winning(&self, marked_numbers: &[Number]) -> bool {
        let is_complete = |v: Vec<Number>| v.iter().all(|n| marked_numbers.contains(n));
        self.data
            .row_iter()
            .any(|r| is_complete(r.iter().copied().collect()))
            || self
                .data
                .column_iter()
                .any(|c| is_complete(c.iter().copied().collect()))
    }

    fn unmarked_numbers(&self, marked_numbers: &[Number]) -> Vec<Number> {
        self.data
            .iter()
            .filter(|n| !marked_numbers.contains(&**n))
            .copied()
            .collect()
    }
}

#[derive(Debug)]
struct Game {
    remaining_numbers: Vec<Number>,
    marked_numbers: Vec<Number>,
    boards: Vec<Board>,
    winning_boards_in_order_of_winning: Vec<BoardIndex>,
}

impl Game {
    fn mark_next(self) -> Option<Game> {
        self.remaining_numbers.first().map(|next| {
            let remaining_numbers = self.remaining_numbers[1..].to_vec();
            let marked_numbers = vec![self.marked_numbers, vec![*next]].concat();
            let new_winning = Game::winning_boards(&self.boards, &marked_numbers)
                .iter()
                .filter(|n| !self.winning_boards_in_order_of_winning.contains(&**n))
                .copied()
                .collect();
            let winning_boards_in_order_of_winning =
                vec![self.winning_boards_in_order_of_winning, new_winning].concat();

            Game {
                remaining_numbers,
                marked_numbers,
                boards: self.boards,
                winning_boards_in_order_of_winning,
            }
        })
    }

    fn winning_boards(boards: &[Board], marked_numbers: &[Number]) -> Vec<BoardIndex> {
        (0..boards.len())
            .filter(|idx| boards[*idx].is_winning(marked_numbers))
            .collect()
    }

    fn last_marked_number(&self) -> Option<Number> {
        self.marked_numbers.last().copied()
    }

    fn unmarked_numbers(&self, board_idx: BoardIndex) -> Vec<Number> {
        self.boards[board_idx].unmarked_numbers(&self.marked_numbers)
    }
}

impl Game {
    fn parse(input: &str) -> Result<Game, Error> {
        let blocks: Vec<&str> = input.split("\n\n").filter(|x| !x.is_empty()).collect();

        let drawn_numbers_str: &str = blocks
            .first()
            .ok_or_else(|| "No drawn numbers found".to_string())?;

        let drawn_numbers: Result<Vec<Number>, Error> = drawn_numbers_str
            .split(',')
            .map(|x| x.parse().map_err(|e| format!("{e} parsing {x}")))
            .collect();

        let boards: Result<Vec<Board>, Error> =
            blocks[1..].iter().map(|s| Board::parse(s)).collect();

        Ok(Game {
            remaining_numbers: drawn_numbers?,
            marked_numbers: vec![],
            boards: boards?,
            winning_boards_in_order_of_winning: vec![],
        })
    }
}

const DATA: &str = include_str!("../../resources/04.txt");

fn answer(game: &Game, board: BoardIndex) -> Result<usize, Error> {
    let last: usize = game
        .last_marked_number()
        .ok_or("Unexpectedly, no last marked number")? as usize;

    let winning_board_unmarked_numbers: usize = game
        .unmarked_numbers(board)
        .iter()
        .map(|x| *x as usize)
        .sum();

    Ok(last * winning_board_unmarked_numbers)
}

fn solve_1(data: &str) -> Result<usize, Error> {
    let mut game = Game::parse(data)?;
    loop {
        game = game.mark_next().ok_or("Failed to win")?;
        let winning = &game.winning_boards_in_order_of_winning;
        if winning.len() > 1 {
            return Err(format!(
                "Did not expect to have multiple winning boards: {:?}",
                winning
            ));
        }

        if let Some(winning) = winning.first() {
            return answer(&game, *winning);
        }
    }
}

fn solve_2(data: &str) -> Result<usize, Error> {
    let mut game = Game::parse(data)?;
    loop {
        game = game.mark_next().ok_or("Failed to win")?;
        let winning = &game.winning_boards_in_order_of_winning;
        if winning.len() == game.boards.len() {
            if let Some(winning) = winning.last() {
                return answer(&game, *winning);
            }
        }
    }
}

fn main() {
    let result_1 = solve_1(DATA);
    println!("Part 1: {result_1:?}");
    let result_2 = solve_2(DATA);
    println!("Part 2: {result_2:?}");
}

#[cfg(test)]
mod tests {
    use super::*;

    const TEST_DATA: &str = include_str!("../../resources/04-test.txt");

    #[test]
    fn test_solve_1_test() {
        assert_eq!(solve_1(TEST_DATA), Ok(4512));
    }

    #[test]
    fn test_solve_1_real() {
        assert_eq!(solve_1(DATA), Ok(89001));
    }

    #[test]
    fn test_solve_2_test() {
        assert_eq!(solve_2(TEST_DATA), Ok(1924));
    }

    #[test]
    fn test_solve_2_real() {
        assert_eq!(solve_2(DATA), Ok(7296));
    }
}
