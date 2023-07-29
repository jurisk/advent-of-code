import { Command, applyCommand } from "./command";

function check(input: string, command: Command, expectedOutput: string) {
  const inputArray = Array.from(input);
  const expectedOutputArray = Array.from(expectedOutput);

  const output = applyCommand(inputArray, command);
  expect(output).toEqual(expectedOutputArray);
}

describe("applyCommand & reverseCommand", () => {
  test("swap position", () => {
    check("abcde", { kind: "swapPosition", x: 4, y: 0 }, "ebcda");
  });

  test("swap letter", () => {
    check("ebcda", { kind: "swapLetter", x: "d", y: "b" }, "edcba");
  });

  test("reverse positions", () => {
    check("edcba", { kind: "reversePositions", x: 0, y: 4 }, "abcde");
  });

  test("rotate left", () => {
    check("abcde", { kind: "rotateSteps", direction: "left", steps: 1 }, "bcdea");
  });

  test("move position 1 to 4", () => {
    check("bcdea", { kind: "movePosition", x: 1, y: 4 }, "bdeac");
  });

  test("move position 3 to 0", () => {
    check("bdeac", { kind: "movePosition", x: 3, y: 0 }, "abdec");
  });

  test("rotate based on position of letter b", () => {
    check("abdec", { kind: "rotateBasedOnPosition", x: "b" }, "ecabd");
  });

  test("rotate based on position of letter d", () => {
    check("ecabd", { kind: "rotateBasedOnPosition", x: "d" }, "decab");
  });
});
