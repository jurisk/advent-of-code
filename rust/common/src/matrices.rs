use pathfinding::matrix::Matrix;

pub trait IsCorner {
    fn is_corner(&self, r: usize, c: usize) -> bool;
}

impl<T> IsCorner for Matrix<T> {
    fn is_corner(&self, r: usize, c: usize) -> bool {
        (r == 0 || r == self.rows - 1) && (c == 0 || c == self.columns - 1)
    }
}

pub trait MapByCoords {
    fn map_by_coords<F, B>(&self, f: F) -> Matrix<B>
    where
        F: Fn((usize, usize)) -> B;
}

impl<A> MapByCoords for Matrix<A> {
    fn map_by_coords<F, B>(&self, f: F) -> Matrix<B>
    where
        F: Fn((usize, usize)) -> B,
    {
        let new_data = self.keys().map(|(r, c)| f((r, c))).collect();
        Matrix::from_vec(self.rows, self.columns, new_data).unwrap()
    }
}
