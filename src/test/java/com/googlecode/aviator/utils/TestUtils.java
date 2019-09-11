package com.googlecode.aviator.utils;

import com.googlecode.aviator.annotation.Function;
import com.googlecode.aviator.annotation.Ignore;
import com.googlecode.aviator.annotation.Import;
import com.googlecode.aviator.annotation.ImportScope;

@Import(ns = "test", scopes = {ImportScope.Static})
public class TestUtils {

  public static String assertNotNull(final String s) {
    return "str";
  }

  public static String assertNotNull(final Number s) {
    return "num";
  }

  public static int add(final int x, final int y) {
    return x + y;
  }

  @Ignore
  public static double dadd(final double a, final double b) {
    return a + b;
  }

  @Function(rename = "fib")
  public static int test(final int i) {
    if (i <= 1) {
      return i;
    }
    return test(i - 1) + test(i - 2);
  }

  @Function(rename = "is_empty")
  public boolean isEmpty(final String s) {
    return s.isEmpty();
  }

  @Function(rename = "is_empty")
  public boolean isEmpty(final TestUtils o) {
    return o == null;
  }

  @Ignore
  public boolean isEmpty(final Number n) {
    return n.longValue() == 0;
  }
}
