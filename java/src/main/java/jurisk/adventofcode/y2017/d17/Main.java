package jurisk.adventofcode.y2017.d17;

public class Main {
    static final int REAL_STEPS = 348;
    static final int PART_1_TIMES = 2017;
    static final int PART_2_TIMES = 50000000;

    public static void main(final String[] args) {
        int result1 = Solution.part1(REAL_STEPS, PART_1_TIMES);
        System.out.printf("Part 1: %d%n", result1);

        int result2 = Solution.part2(REAL_STEPS, PART_2_TIMES);
        System.out.printf("Part 2: %d%n", result2);
    }
}
