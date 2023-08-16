import 'dart:io';

import 'package:dart/2017/day21.dart';
import 'package:test/test.dart';

void main() {
  final testData = File('input/2017/day21-test.txt').readAsStringSync();
  final realData = File('input/2017/day21.txt').readAsStringSync();

  test('Part 1 Test', () {
    expect(solution(testData, 2), 12);
  });

  test('Part 1 Real', () {
    expect(solution(realData, 5), 120);
  });

  test('Part 2 Real', () {
    expect(solution(realData, 18), 2204099);
  }, skip: 'Slow');
}
