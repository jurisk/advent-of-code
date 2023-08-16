import 'package:dart/grid.dart';

extension GridBoolExtensions on Grid<bool> {
  Grid<bool> mapAccordingToRules(List<Rule> rules) =>
      rules.firstWhere((rule) => rule.matches(this)).to;

  Grid<bool> nextState(List<Rule> rules) {
    print("Size: $width ✕ $height");

    Grid<Grid<bool>> determineChoppedUp(Grid<bool> g) {
      if (g.canBeChoppedUpBy(2)) return g.chopUpBy(2);
      if (g.canBeChoppedUpBy(3)) return g.chopUpBy(3);
      throw StateError("Failed to figure out how to chop up grid $g");
    }

    final Grid<Grid<bool>> choppedUp = determineChoppedUp(this);
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
    assert(elements.length == 2);
    return Rule(parseGrid(elements[0]), parseGrid(elements[1]));
  }

  List<Grid<bool>> variations() =>
      from.rotations() + from.mirrored().rotations();

  bool matches(Grid<bool> grid) =>
      variations().any((variation) => variation == grid);
}

const String startingPattern = ".#./..#/###";

int solution(String data, int iterations) {
  final rules = data.split("\n").map((line) => Rule.parse(line)).toList();
  final Grid<bool> start = parseGrid(startingPattern);

  final Grid<bool> state =
      Iterable<int>.generate(iterations).fold(start, (currentState, idx) {
    print("Step $idx...");
    return currentState.nextState(rules);
  });

  return state.count((elem) => elem);
}
