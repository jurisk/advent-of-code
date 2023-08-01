package jurisk.adventofcode.y2017.d17;

import static jurisk.util.Loops.doNTimes;

public class Solution {
    public static int part1(final int steps, final int times) {
        final var buffer = new CircularBuffer();
        doNTimes(times, () -> buffer.next(steps));
        return buffer.valueAfter(times);
    }

    public static int part2(final int steps, final int times) {
        var result = 0;
        var index = 0;

        for (int i = 0; i < times; i++) {
            final var size = i + 1;
            index = (index + steps) % size + 1;
            if (index == 1) { // the value after 0
                result = size;
            }
        }

        return result;
    }
}
