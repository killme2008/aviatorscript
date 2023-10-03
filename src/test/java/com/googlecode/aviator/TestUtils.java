package com.googlecode.aviator;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.List;
import java.util.Objects;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.TypeUtils;
import junit.framework.AssertionFailedError;

/**
 * Test helper
 *
 * @author dennis
 *
 */
public class TestUtils {

  public static Env getTestEnv() {
    Env env = new Env();
    env.setInstance(AviatorEvaluator.getInstance());
    return env;
  }

  static public void assertEquals(final double expected, final double actual, final double delta) {
    assertEquals(null, expected, actual, delta);
  }

  static public void assertEquals(final String message, final double expected, final double actual,
      final double delta) {
    if (Double.compare(expected, actual) == 0) {
      return;
    }
    if (!(Math.abs(expected - actual) <= delta)) {
      throw new AssertionFailedError("Expect " + expected + ", but was " + actual);
    }
  }

  static public void assertSame(final String message, final Object expect, final Object actual) {
    if (expect != actual) {
      throw new AssertionFailedError(message);
    }
  }

  static public void assertSame(final Object expect, final Object actual) {
    if (expect != actual) {
      throw new AssertionFailedError(
          "Expect " + expect + " the same with " + actual + ", but it's not.");
    }
  }

  public static void assertListEquals(final List<?> expected, final List<?> real) {
    if (expected.size() != real.size()) {
      throw new AssertionFailedError("Expect " + expected + " , but actual was " + real);
    }

    for (int i = 0; i < expected.size(); i++) {
      assertEquals(expected.get(i), real.get(i));
    }
  }

  public static void assertArrayEquals(final Object[] expected, final Object[] real) {
    if (expected.length != real.length) {
      throw new AssertionFailedError(
          "Expect " + Arrays.toString(expected) + " , but actual was " + Arrays.toString(real));
    }

    for (int i = 0; i < expected.length; i++) {
      assertEquals(expected[i], real[i]);
    }
  }

  public static void assertEquals(final Object expected, final Object real) {
    if (expected == real) {
      return;
    }

    if (expected instanceof Number && real instanceof Number) {
      if (TypeUtils.isDouble(expected) && TypeUtils.isDouble(real)) {
        double delta = ((Number) expected).doubleValue() - ((Number) real).doubleValue();
        if (delta < 0.00001) {
          return;
        }
      }

      if (TypeUtils.isLong(expected) && TypeUtils.isLong(real)) {
        if (((Number) expected).longValue() == ((Number) real).longValue()) {
          return;
        }
      }

      if (TypeUtils.isDecimal(expected) && TypeUtils.isDecimal(real)) {
        if (((BigDecimal) expected).compareTo((BigDecimal) real) == 0) {
          return;
        }
      }
    }

    if (Objects.equals(expected, real)) {
      return;
    }
    throw new AssertionFailedError(
        "Expect " + expected + "(" + (expected == null ? null : expected.getClass())
            + "), but actual was " + real + "(" + (real == null ? null : real.getClass()) + ")");
  }

  public static void assertTrue(final Boolean bool) {
    if (!bool) {
      throw new AssertionFailedError("Expect true, but actual was false");
    }
  }

  public static void assertFalse(final Boolean bool) {
    if (bool) {
      throw new AssertionFailedError("Expect false, but actual was true");
    }
  }

  public static void assertNull(final Object obj) {
    if (obj != null) {
      throw new AssertionFailedError("Expect null, but actual was not");
    }
  }

  public static void assertNotNull(final Object obj) {
    if (obj == null) {
      throw new AssertionFailedError("Expect not null, but actual was null");
    }
  }

  public static void fail() {
    throw new AssertionFailedError("Expect not run here");
  }
}
