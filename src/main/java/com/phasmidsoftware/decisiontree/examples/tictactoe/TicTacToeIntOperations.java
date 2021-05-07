package com.phasmidsoftware.decisiontree.examples.tictactoe;

public class TicTacToeIntOperations {

    /**
     * Parse and array of ints, each one either 0 (open), 1 (X), or 2 (0).
     *
     * @param a an array of 9 ints.
     * @return the encoding of the board.
     */
    public static int parse(int[] a) {
        int result = 0;
        for (int i = 0; i < 9; i++) result = result << 2 | a[i];
        return result << 14;
    }

    public static int[][] asArray(int x) {
        // CONSIDER is there a more efficient way to count the bits?
        int[][] result = new int[3][3];
        for (int i = 0; i < 3; i++)
            for (int j = 0; j < 3; j++) {
                int y = x & 0xC0000000;
                result[i][j] = y >> 30;
                x = x << 2;
            }
        return result;
    }

    public static int[] open(int x) {
        // CONSIDER is there a more efficient way to count the bits?
        int[] empty = new int[9];
        int count = 0;
        for (int i = 0; i < 9; i++) {
            int y = x & 0xC0000000;
            if (y == 0xC0000000 || y == 0x00000000) empty[count++] = i;
            x = x << 2;
        }
        int[] result = new int[count];
        System.arraycopy(empty, 0, result, 0, count);
        return result;
    }

    public static String render(int x) {
        StringBuilder sb = new StringBuilder();
        for (int i = 0; i < 3; i++) {
            for (int j = 0; j < 3; j++) {
                int y = ((x & 0xC0000000) >> 30) & 0x3;
                sb.append(y == 1 ? 'X' : y == 2 ? '0' : '.');
                x = x << 2;
            }
            sb.append('\n');
        }
        return sb.toString();
    }

    public static int play(int x, boolean player, int i, int j) {
        int bits = (player ? 1 : 2) << (62 - i * 6 - j * 2);
        return x | bits;
    }

    public static int transposeBoard(int x) {
        long y = (long) x & 0x00000000FFFFFFFFL;
        long z = y | flip(y, 0, 0) | flip(y, 1, 2) | flip(y, 2, 4) | flip(y, 3, -2) | flip(y, 4, 0) | flip(y, 5, 2) | flip(y, 6, -4) | flip(y, 7, -2) | flip(y, 8, 0);
        return (int) (z >> 32);
    }

    public static int row(int x, int i) {
        int shift = 26 - 6 * i;
        return x >> shift & 0x3F;
    }

    public static int diagonal(int x) {
        long y = (long) x & 0x00000000FFFFFFFFL;
        long z = y | flip(y, 0, 0) | flip(y, 4, -3) | flip(y, 8, -6);
        return (int) ((z >> 58) & 0x3F);
    }

    public static int rowLine(int x) {
        int y1 = x ^ 0x15; // XXX
        int y2 = x ^ 0x2A; // 000
        return y2 == 0 ? 2 : y1 == 0 ? 1 : 0;
    }

    public static int rowLinePending(int x) {
        int y1 = x ^ 0x15; // XXX
        int y2 = x ^ 0x2A; // 000
        return y2 == 0 ? 2 : y1 == 0 ? 1 : 0;
    }

    private static long flip(long y, int i, int j) {
        return (y & (0xC0000000L) >> i * 2) << (32 - j * 2);
    }
}
