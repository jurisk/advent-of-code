package jurisk.adventofcode.y2020

import jurisk.adventofcode.AdventAppSpec

class Advent22Spec extends AdventAppSpec(Advent22):
  runTest("test00", testData00, 306, 291)
  runTest("real", realData, 34566, 31854)
