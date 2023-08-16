extension ChunksExtension<T> on List<T> {
  List<List<T>> chunks(int n) {
    return [
      for (int i = 0; i < length; i += n)
        sublist(i, i + n > length ? length : i + n)
    ];
  }
}
