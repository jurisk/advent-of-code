use advent_of_code_common::circular::Circular;

#[allow(clippy::module_name_repetitions)]
pub fn knot_hash_make_folds(data: &mut Circular<u8>, fold_lengths: &[u8], rounds: usize) {
    let mut current: usize = 0;
    let mut skip_size: usize = 0;
    for _round in 0 .. rounds {
        for i in fold_lengths {
            let skip_length = *i as usize;
            data.reverse_slice(current, skip_length);
            current += skip_length + skip_size;
            skip_size += 1;
        }
    }
}

const STANDARD_LENGTH_SUFFIXES: [u8; 5] = [17, 31, 73, 47, 23];

#[must_use]
#[allow(clippy::module_name_repetitions)]
/// # Panics
///
/// Should not panic
pub fn knot_hash_as_u8(input: &str) -> Vec<u8> {
    let fold_lengths: Vec<u8> = [input.as_bytes(), &STANDARD_LENGTH_SUFFIXES].concat();
    let mut data = (0 ..= 255).collect();
    knot_hash_make_folds(&mut data, &fold_lengths, 64);
    debug_assert_eq!(data.len(), 256);
    let dense_hash: Vec<u8> = data
        .vec
        .chunks(16)
        .map(|x| x.iter().copied().reduce(|x, y| x ^ y).unwrap())
        .collect();
    debug_assert_eq!(dense_hash.len(), 16);
    dense_hash
}

#[must_use]
#[allow(clippy::module_name_repetitions)]
pub fn knot_hash_as_string(input: &str) -> String {
    let dense_hash = knot_hash_as_u8(input);
    let result: String = dense_hash.iter().map(|x| format!("{x:02x}")).collect();
    debug_assert_eq!(result.len(), 32);
    result
}
