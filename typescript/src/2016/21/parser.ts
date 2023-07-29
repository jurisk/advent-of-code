import { Command } from "./command";

function parseCommand(commandStr: string): Command {
  type Parser = (commandStr: string) => Command | null;

  const parsers: Parser[] = [
    (commandStr) => {
      const match = commandStr.match(/^swap position (\d+) with position (\d+)$/);
      return match ? { kind: "swapPosition", x: parseInt(match[1]), y: parseInt(match[2]) } : null;
    },
    (commandStr) => {
      const match = commandStr.match(/^swap letter (\w) with letter (\w)$/);
      return match ? { kind: "swapLetter", x: match[1], y: match[2] } : null;
    },
    (commandStr) => {
      const match = commandStr.match(/^rotate (left|right) (\d+) step/);
      return match
        ? {
            kind: "rotateSteps",
            direction: match[1] as "left" | "right",
            steps: parseInt(match[2]),
          }
        : null;
    },
    (commandStr) => {
      const match = commandStr.match(/^rotate based on position of letter (\w)$/);
      return match ? { kind: "rotateBasedOnPosition", x: match[1] } : null;
    },
    (commandStr) => {
      const match = commandStr.match(/^reverse positions (\d+) through (\d+)$/);
      return match
        ? {
            kind: "reversePositions",
            x: parseInt(match[1]),
            y: parseInt(match[2]),
          }
        : null;
    },
    (commandStr) => {
      const match = commandStr.match(/^move position (\d+) to position (\d+)$/);
      return match ? { kind: "movePosition", x: parseInt(match[1]), y: parseInt(match[2]) } : null;
    },
  ];

  for (const parser of parsers) {
    const result = parser(commandStr);
    if (result) return result;
  }

  throw new Error(`Invalid command: '${commandStr}'`);
}

export function parse(data: string): Command[] {
  const lines = data.split(/\r?\n/);
  return lines.map((line) => parseCommand(line));
}
