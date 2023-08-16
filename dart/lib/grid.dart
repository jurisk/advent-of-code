import 'package:collection/collection.dart';
import 'package:dart/util.dart';

class Grid<T> {
  final int width, height;
  final List<List<T>> values;

  Grid(this.width, this.height, this.values);

  Grid.fromValues(this.values)
      : assert(values.every((row) => row.length == values[0].length),
            "All rows should have the same width."),
        width = values[0].length,
        height = values.length;

  factory Grid.parse(
      String input, String delimiter, T Function(String) parseCell) {
    final contents = input
        .split(delimiter)
        .map((row) => row.split('').map(parseCell).toList())
        .toList();

    return Grid.fromValues(contents);
  }

  Grid<R> map<R>(R Function(T) function) {
    return Grid<R>(width, height,
        values.map((row) => row.map(function).toList()).toList());
  }

  int count(bool Function(T) predicate) {
    return values.expand((row) => row).where(predicate).length;
  }

  bool canBeChoppedUpBy(int n) => width % n == 0 && height % n == 0;

  // Returns this Grid chopped up into smaller Grid-s, each with n x n size
  Grid<Grid<T>> chopUpBy(int n) {
    assert(width % n == 0);
    assert(height % n == 0);

    return Grid<Grid<T>>.fromValues(values
        .chunks(n)
        .map((rowChunk) => List.generate(
            width ~/ n,
            (j) => Grid<T>.fromValues(rowChunk
                .map((row) => row.sublist(j * n, (j + 1) * n))
                .toList())))
        .toList());
  }

  Grid<T> mirrored() {
    final reversed = values.map((row) => row.reversed.toList()).toList();
    return Grid(width, height, reversed);
  }

  List<Grid<T>> rotations() {
    final rotation90 = rotateClockwise90();
    final rotation180 = rotation90.rotateClockwise90();
    final rotation270 = rotation180.rotateClockwise90();

    return [this, rotation90, rotation180, rotation270];
  }

  Grid<T> rotateClockwise90() => Grid<T>.fromValues(
      transposed().map((row) => row.reversed.toList()).toList());

  List<List<T>> transposed() => List<List<T>>.generate(
      width, (i) => List<T>.generate(height, (j) => values[j][i]));

  @override
  String toString() => 'Grid($values)';

  @override
  bool operator ==(Object other) {
    return identical(this, other) ||
        other is Grid<T> &&
            runtimeType == other.runtimeType &&
            DeepCollectionEquality().equals(values, other.values);
  }

  @override
  int get hashCode => values.hashCode;
}

extension NestedGridMerge<T> on Grid<Grid<T>> {
  Grid<T> merge() => Grid.fromValues(
      values.expand((rowOfGrids) => _concatRowOfGrids(rowOfGrids)).toList());

  Iterable<List<T>> _concatRowOfGrids(List<Grid<T>> rowOfGrids) {
    final height = rowOfGrids.first.height;

    return Iterable.generate(
        height, (i) => rowOfGrids.expand((g) => g.values[i]).toList());
  }
}
