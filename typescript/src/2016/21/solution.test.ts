import fs from "fs";
import path from "path";
import { part1, part2 } from "./solution";
import { Command } from "./command";
import { parse } from "./parser";

describe("Solution", () => {
  let testData: string;
  let realData: string;
  let parsedTest: Command[];
  let parsedReal: Command[];

  beforeAll(() => {
    testData = fs.readFileSync(path.resolve(__dirname, "test.txt"), "utf8");
    realData = fs.readFileSync(path.resolve(__dirname, "real.txt"), "utf8");

    parsedTest = parse(testData);
    parsedReal = parse(realData);
  });

  test("part 1 test data", () => {
    const result = part1(parsedTest, "abcde");
    expect(result).toBe("decab");
  });

  test("part 1 real data", () => {
    const result = part1(parsedReal, "abcdefgh");
    expect(result).toBe("aefgbcdh");
  });

  test("part 2 test data 1", () => {
    const result = part2(parsedTest, "decab");
    expect(result.sort()).toEqual(["deabc", "abcde"].sort());
  });

  test("part 2 test data 2", () => {
    const result = part2(parsedReal, "aefgbcdh");
    expect(result).toEqual(["abcdefgh"]);
  });

  test("part 2 real data", () => {
    const result = part2(parsedReal, "fbgdceah");
    expect(result).toEqual(["egcdahbf"]);
  });
});
