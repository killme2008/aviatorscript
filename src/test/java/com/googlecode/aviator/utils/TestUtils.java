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

  public static String join(final String s, final String... args) {
    return s + "," + join(args);
  }

  public static String join(final String... args) {
    if (args == null || args.length == 0) {
      return "";
    }
    StringBuilder sb = new StringBuilder();
    boolean wasFirst = true;
    for (int i = 0; i < args.length; i++) {
      if (wasFirst) {
        sb.append(args[i]);
        wasFirst = false;
      } else {
        sb.append(",").append(args[i]);
      }
    }
    return sb.toString();
  }

  public static String join2(final String... args) {
    return join(args);
  }
}
