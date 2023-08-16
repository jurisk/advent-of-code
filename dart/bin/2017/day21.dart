import 'dart:io';

import 'package:dart/2017/day21.dart' as day21;

Future<void> main() async {
  final data = await File('input/2017/day21.txt').readAsString();
  final result1 = day21.solution(data, 5);
  print('Part 1: $result1');

  final result2 = day21.solution(data, 18);
  print('Part 2: $result2');
}
