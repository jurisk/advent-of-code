extension ListSum on List<int> {
  int sum() => fold(0, (a, b) => a + b);
}

extension ChunksExtension<T> on List<T> {
  List<List<T>> chunks(int n) {
    return [
      for (int i = 0; i < length; i += n)
        sublist(i, i + n > length ? length : i + n)
    ];
  }
}
