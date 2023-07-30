import { assertNever } from "../../util/misc";

export enum Direction {
  Left,
  Right,
}

type SwapPosition = {
  kind: "swapPosition";
  x: number;
  y: number;
};

type SwapLetter = {
  kind: "swapLetter";
  x: string;
  y: string;
};

type RotateSteps = {
  kind: "rotateSteps";
  direction: Direction;
  steps: number;
};

type RotateBasedOnPosition = {
  kind: "rotateBasedOnPosition";
  x: string;
};

type ReversePositions = {
  kind: "reversePositions";
  x: number;
  y: number;
};

type MovePosition = {
  kind: "movePosition";
  x: number;
  y: number;
};

export type Command = SwapPosition | SwapLetter | RotateSteps | RotateBasedOnPosition | ReversePositions | MovePosition;

export function applyCommand(input: readonly string[], command: Command): readonly string[] {
  switch (command.kind) {
    case "swapPosition": {
      const swapped = [...input];
      [swapped[command.x], swapped[command.y]] = [input[command.y], input[command.x]];
      return swapped;
    }

    case "swapLetter": {
      return input.map((char) => {
        if (char === command.x) return command.y;
        if (char === command.y) return command.x;
        return char;
      });
    }

    case "rotateSteps": {
      const steps = command.steps % input.length;
      switch (command.direction) {
        case Direction.Right:
          return [...input.slice(-steps), ...input.slice(0, -steps)];
        case Direction.Left:
          return [...input.slice(steps), ...input.slice(0, steps)];
        default:
          return assertNever(command.direction);
      }
    }

    case "rotateBasedOnPosition": {
      const index = input.indexOf(command.x);
      const rotateSteps = 1 + index + (index >= 4 ? 1 : 0);
      return applyCommand(input, {
        kind: "rotateSteps",
        direction: Direction.Right,
        steps: rotateSteps,
      });
    }

    case "reversePositions": {
      return [
        ...input.slice(0, command.x),
        ...input.slice(command.x, command.y + 1).reverse(),
        ...input.slice(command.y + 1),
      ];
    }

    case "movePosition": {
      const moved = [...input];
      const letter = moved[command.x];
      moved.splice(command.x, 1);
      moved.splice(command.y, 0, letter);
      return moved;
    }

    default:
      return assertNever(command);
  }
}
