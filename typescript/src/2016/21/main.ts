import * as fs from "fs";
import { parse } from "./parser";
import { part1, part2 } from "./solution";

const realData: string = fs.readFileSync("src/2016/21/real.txt", "utf8");
const parsedReal = parse(realData);

const realResult1 = part1(parsedReal, "abcdefgh");
console.log(`Part 1: ${realResult1}`);

const realResult2 = part2(parsedReal, "fbgdceah");
console.log(`Part 2: ${realResult2}`);
