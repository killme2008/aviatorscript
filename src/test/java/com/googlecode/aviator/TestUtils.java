package com.googlecode.aviator;

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

  public static void assertEquals(final Object expected, final Object real) {
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
    }

    if (Objects.equals(expected, real)) {
      return;
    }
    throw new AssertionFailedError("Expect " + expected + "(" + expected.getClass()
        + "), but actual was " + real + "(" + real.getClass() + ")");
  }
}
