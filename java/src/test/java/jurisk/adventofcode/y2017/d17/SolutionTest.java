package jurisk.adventofcode.y2017.d17;

import org.testng.annotations.Test;

import static jurisk.adventofcode.y2017.d17.Main.*;
import static org.testng.Assert.assertEquals;
import static jurisk.adventofcode.y2017.d17.Solution.*;

public class SolutionTest {
    private static final int TEST_STEPS = 3;

    @Test
    public void part1Test1() {
        assertEquals(part1(TEST_STEPS, 9), 5);
    }

    @Test
    public void part1Test2() {
        assertEquals(part1(TEST_STEPS, PART_1_TIMES), 638);
    }

    @Test
    public void part1Real() {
        assertEquals(part1(REAL_STEPS, PART_1_TIMES), 417);
    }

    @Test
    public void part2Test() {
        assertEquals(part2(TEST_STEPS, 1), 1);
        assertEquals(part2(TEST_STEPS, 2), 2);
        assertEquals(part2(TEST_STEPS, 3), 2);
        assertEquals(part2(TEST_STEPS, 4), 2);
        assertEquals(part2(TEST_STEPS, 5), 5);
        assertEquals(part2(TEST_STEPS, 6), 5);
        assertEquals(part2(TEST_STEPS, 7), 5);
        assertEquals(part2(TEST_STEPS, 8), 5);
        assertEquals(part2(TEST_STEPS, 9), 9);
    }

    @Test
    public void part2Real() {
        assertEquals(part2(REAL_STEPS, PART_2_TIMES), 34_334_221);
    }
}
