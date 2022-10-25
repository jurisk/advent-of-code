pub fn head_tail<T>(slice: &[T]) -> (Option<&T>, &[T]) {
    if slice.is_empty() {
        (None, &[])
    } else {
        (Some(&slice[0]), &slice[1..])
    }
}
