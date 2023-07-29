import { arraysEqual } from "../../util/collections";
import { Command, applyCommand } from "./command";
import { Permutation } from "js-combinatorics";

function applyCommands(commands: Command[], input: readonly string[]): readonly string[] {
  return commands.reduce(applyCommand, input);
}

export function part1(commands: Command[], input: string): string {
  const inputArray = Array.from(input);
  const result = applyCommands(commands, inputArray);
  return result.join("");
}

export function part2(commands: Command[], input: string): string[] {
  const inputArray = Array.from(input);
  const permutations = new Permutation(inputArray);

  const results = [];

  for (const permutation of permutations) {
    const output = applyCommands(commands, permutation);

    if (arraysEqual(output, inputArray)) {
      results.push(permutation.join(""));
    }
  }

  return results;
}
