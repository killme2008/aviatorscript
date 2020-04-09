package com.googlecode.aviator.utils;

import java.io.IOException;
import java.io.Reader;

/**
 * Some helper methods.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class Utils {
  private Utils() {

  }

  public static String readFully(final Reader reader) throws IOException {
    final char[] arr = new char[16 * 1024];
    final StringBuilder buf = new StringBuilder();
    int numChars;

    while ((numChars = reader.read(arr, 0, arr.length)) > 0) {
      buf.append(arr, 0, numChars);
    }

    return buf.toString();
  }
}
