import 'package:dart/grid.dart';

extension GridBoolExtensions on Grid<bool> {
  Grid<bool> mapAccordingToRules(List<Rule> rules) =>
      rules.firstWhere((rule) => rule.matches(this)).to;

  Grid<bool> nextState(List<Rule> rules) {
    print("Size: $width ✕ $height");

    Grid<Grid<bool>> determineChoppedUp(List<int> options, Grid<bool> g) {
      final n = options.firstWhere((n) => g.canBeChoppedUpBy(n),
          orElse: () =>
              throw StateError("Failed to figure out how to chop up grid $g"));
      return chopUpBy(n);
    }

    final Grid<Grid<bool>> choppedUp = determineChoppedUp([2, 3], this);
    final Grid<Grid<bool>> mapped =
        choppedUp.map((grid) => grid.mapAccordingToRules(rules));
    return mapped.merge();
  }
}

bool parseChar(String ch) {
  return switch (ch) {
    '.' => false,
    '#' => true,
    _ => throw ArgumentError("Unknown character $ch")
  };
}

Grid<bool> parseGrid(String input) => Grid<bool>.parse(input, "/", parseChar);

class Rule {
  final Grid<bool> from;
  final Grid<bool> to;

  Rule(this.from, this.to);

  factory Rule.parse(String line) {
    final elements = line.split(" => ");
    return (elements.length == 2)
        ? Rule(parseGrid(elements[0]), parseGrid(elements[1]))
        : throw ArgumentError("Invalid rule format: $line");
  }

  List<Grid<bool>> variations() =>
      from.rotations() + from.mirrored().rotations();

  bool matches(Grid<bool> grid) =>
      variations().any((variation) => variation == grid);
}

const String startingPattern = ".#./..#/###";

int solution(String data, int iterations) {
  final rules = data.split("\n").map(Rule.parse).toList(growable: false);
  final Grid<bool> start = parseGrid(startingPattern);

  final Grid<bool> state =
      Iterable<int>.generate(iterations).fold(start, (currentState, idx) {
    print("Step $idx...");
    return currentState.nextState(rules);
  });

  return state.count((elem) => elem);
}
