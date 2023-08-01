package jurisk.util;

public class Loops {
    public static void doNTimes(final int n, final Runnable action) {
        for (int i = 0; i < n; i++) {
            action.run();
        }
    }
}
