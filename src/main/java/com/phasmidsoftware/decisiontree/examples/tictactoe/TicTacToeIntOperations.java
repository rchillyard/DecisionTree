package com.phasmidsoftware.decisiontree.examples.tictactoe;

@SuppressWarnings("CommentedOutCode")
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

//    /**
//     * Unused method.
//     *
//     * @param x
//     * @return
//     */
//    public static int[][] asArray(int x) {
//        // CONSIDER is there a more efficient way to count the bits?
//        int[][] result = new int[3][3];
//        for (int i = 0; i < 3; i++)
//            for (int j = 0; j < 3; j++) {
//                int y = x & 0xC0000000;
//                result[i][j] = y >> 30;
//                x = x << 2;
//            }
//        return result;
//    }

    /**
     * Method to yield an array of the vacant (or open) cells.
     * The sequence of the resulting array is top-left (index 0), top-middle, top-right, mid-left, mid-middle, etc.
     * up to bottom-right (index 8).
     *
     * @param x a Board.
     * @return an array of index (int) elements, each of which represents a vacant space.
     */
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

    /**
     * Method to render a Board as three lines of X, 0, and . separated by newlines.
     *
     * @param x a Board.
     * @return a String, which ends in a newline.
     */
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

    /**
     * Make a play on a Board at a particular cell (x, y).
     *
     * @param x      the Board on which to play.
     * @param player the player (true if X, false if 0).
     * @param i      the row index.
     * @param j      the column index.
     * @return the bits of x OR'd with a bit representing the play.
     */
    public static int play(int x, boolean player, int i, int j) {
        return x | (player ? 1 : 2) << (62 - i * 6 - j * 2);
    }

    /**
     * Method to transpose a board.
     * This operation is a hFlip followed by a rotate.
     * We use this method when we need a column.
     *
     * @param x the bits of a Board.
     * @return a Board which is the transpose of x.
     */
    public static int transposeBoard(int x) {
        return rotate(hFlip(x));
    }

    /**
     * Perform a flip about a horizontal line.
     * This transforms, for example, R0 into L0.
     *
     * @param x the bit pattern to be flipped.
     * @return the flipped bit pattern.
     */
    public static int hFlip(int x) {
        return row(x, 2) << 26 | row(x, 1) << 20 | row(x, 0) << 14;
    }

    /**
     * Perform a 90 degree (clockwise) rotation about a the center cell.
     * This transforms, for example, R0 into R1 or L0 into L1.
     *
     * @param x the bit pattern to be rotated.
     * @return the rotated bit pattern.
     */
    public static int rotate(int x) {
        long y = (long) x & 0x00000000FFFFFFFFL;
        long z = flip(y, 0, 2) | flip(y, 1, 4) | flip(y, 2, 6) | flip(y, 3, -2) | flip(y, 4, 0) | flip(y, 5, 2) | flip(y, 6, -6) | flip(y, 7, -4) | flip(y, 8, -2);
        return (int) (z >> 32);
    }

    /**
     * Method to yield row from a Board.
     * NOTE: to yield a column, we first transpose the board and then get a row.
     *
     * @param x the bits of a Board.
     * @return a Row.
     */
    public static int row(int x, int i) {
        int shift = 26 - 6 * i;
        return x >> shift & 0x3F;
    }

    /**
     * Method to yield a diagonal from a Board.
     *
     * @param x the bits of a Board.
     * @return a Row.
     */
    public static int diagonal(int x) {
        long y = (long) x & 0x00000000FFFFFFFFL;
        long z = y | flip(y, 0, 0) | flip(y, 4, -3) | flip(y, 8, -6);
        return (int) ((z >> 58) & 0x3F);
    }

    /**
     * Method to detect a line (i.e. three in a row, column, or diagonal).
     *
     * @param x the bits of a Row (not a Board).
     * @return 0 if no line, 1 if X has a line, 2 if 0 has a line.
     */
    public static int rowLine(int x) {
        if (x == 0) return 0;
        int y1 = x ^ 0x15; // XXX
        int y2 = x ^ 0x2A; // 000
        return y2 == 0 ? 2 : y1 == 0 ? 1 : 0;
    }

    /**
     * Method to detect a potential line (i.e. two in a row, column, or diagonal).
     *
     * @param x the bits of a Row (not a Board).
     * @return 0 if no line pending, 1 if X has two cells, 2 if 0 has two cells.
     */
    public static int rowLinePending(int x) {
        return switch (x) {
            case 0x11, 0x14, 0x05 -> 1;
            case 0x22, 0x28, 0x0A -> 2;
            default -> 0;
        };
    }

    /**
     * Method to move (flip) a pair of bits.
     *
     * @param y the bits of the given long.
     * @param i the index of the bit pair, counting from the left and starting with 0.
     * @param j the relative position of the bit pair to which the y bits are to be moved.
     * @return the flipped value.
     */
    private static long flip(long y, int i, int j) {
        return (y & (0xC0000000L) >> i * 2) << (32 - j * 2);
    }
}
