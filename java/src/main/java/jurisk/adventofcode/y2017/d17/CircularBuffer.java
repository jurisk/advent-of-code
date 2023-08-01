package jurisk.adventofcode.y2017.d17;

import java.util.HashMap;
import java.util.Map;

import static jurisk.util.Loops.doNTimes;

class CircularBuffer {
    private final Map<Integer, Integer> map = new HashMap<>(Map.of(0, 0));
    private int currentIndex = 0;

    @Override
    public java.lang.String toString() {
        final var result = new StringBuilder();
        var index = 0;
        for (int i = 0; i < map.size(); i++) {
            if (index == currentIndex) {
                result.append('(').append(index).append(')');
            } else {
                result.append(' ').append(index).append(' ');
            }
            result.append(' ');
            index = map.get(index);
        }

        return result.append('\n').toString();
    }

    public void next(final int steps) {
        final var valueToInsert = map.size();
        final var effectiveSteps = steps % map.size();

        doNTimes(effectiveSteps, () -> currentIndex = map.get(currentIndex));

        final var after = map.get(currentIndex);
        map.put(currentIndex, valueToInsert);
        map.put(valueToInsert, after);

        currentIndex = valueToInsert;
    }

    public int valueAfter(final int n) {
        return map.get(n);
    }
}
