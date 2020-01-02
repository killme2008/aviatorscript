package com.googlecode.aviator.utils;

/**
 * Non-threadsafe temporal variable name generator
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class VarNameGenerator {
  private long number;

  private static final String TEMP_VAR_PREFIX = "A_";

  private static final int TEMP_LEN =
      TEMP_VAR_PREFIX.length() + String.valueOf(Long.MAX_VALUE).length();

  private final char[] chars;

  private int count;

  public VarNameGenerator() {
    this(0);
  }

  public VarNameGenerator(final long n) {
    this.number = n;
    this.chars = new char[TEMP_LEN];
    TEMP_VAR_PREFIX.getChars(0, TEMP_VAR_PREFIX.length(), this.chars, 0);
    this.count = TEMP_VAR_PREFIX.length();
  }

  /**
   * Generate a temporal variable name.
   *
   * @return
   */
  public final String gen() {
    long n = this.number++;
    if (n < 0) {
      this.number = 0;
      n = this.number++;
    }

    append(n);
    return new String(this.chars, 0, this.count);
  }

  // Moved from Long.stringSize
  static int stringSize(final long x) {
    long p = 10;
    for (int i = 1; i < 19; i++) {
      if (x < p) {
        return i;
      }
      p = 10 * p;
    }
    return 19;
  }


  final static char[] DigitOnes = {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2',
      '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1',
      '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0',
      '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',
      '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '1', '2', '3', '4', '5', '6', '7', '8',
      '9', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9',};

  final static char[] digits =
      {'0', '1', '2', '3', '4', '5', '6', '7', '8', '9', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h',
          'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z'};

  final static char[] DigitTens = {'0', '0', '0', '0', '0', '0', '0', '0', '0', '0', '1', '1', '1',
      '1', '1', '1', '1', '1', '1', '1', '2', '2', '2', '2', '2', '2', '2', '2', '2', '2', '3', '3',
      '3', '3', '3', '3', '3', '3', '3', '3', '4', '4', '4', '4', '4', '4', '4', '4', '4', '4', '5',
      '5', '5', '5', '5', '5', '5', '5', '5', '5', '6', '6', '6', '6', '6', '6', '6', '6', '6', '6',
      '7', '7', '7', '7', '7', '7', '7', '7', '7', '7', '8', '8', '8', '8', '8', '8', '8', '8', '8',
      '8', '9', '9', '9', '9', '9', '9', '9', '9', '9', '9',};

  // moved from Long.getChars
  static void getCharsFromLong(long i, final int index, final char[] buf) {
    long q;
    int r;
    int charPos = index;
    char sign = 0;

    if (i < 0) {
      sign = '-';
      i = -i;
    }

    // Get 2 digits/iteration using longs until quotient fits into an int
    while (i > Integer.MAX_VALUE) {
      q = i / 100;
      // really: r = i - (q * 100);
      r = (int) (i - ((q << 6) + (q << 5) + (q << 2)));
      i = q;
      buf[--charPos] = DigitOnes[r];
      buf[--charPos] = DigitTens[r];
    }

    // Get 2 digits/iteration using ints
    int q2;
    int i2 = (int) i;
    while (i2 >= 65536) {
      q2 = i2 / 100;
      // really: r = i2 - (q * 100);
      r = i2 - ((q2 << 6) + (q2 << 5) + (q2 << 2));
      i2 = q2;
      buf[--charPos] = DigitOnes[r];
      buf[--charPos] = DigitTens[r];
    }

    // Fall thru to fast mode for smaller numbers
    // assert(i2 <= 65536, i2);
    for (;;) {
      q2 = (i2 * 52429) >>> (16 + 3);
      r = i2 - ((q2 << 3) + (q2 << 1)); // r = i2-(q2*10) ...
      buf[--charPos] = digits[r];
      i2 = q2;
      if (i2 == 0) {
        break;
      }
    }
    if (sign != 0) {
      buf[--charPos] = sign;
    }
  }

  private void append(final long l) {
    int appendedLength = stringSize(l);
    int spaceNeeded = TEMP_VAR_PREFIX.length() + appendedLength;
    getCharsFromLong(l, spaceNeeded, this.chars);
    this.count = spaceNeeded;
  }
}
