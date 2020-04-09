package com.googlecode.aviator.utils;

import java.io.IOException;
import java.io.Reader;
import java.security.MessageDigest;

/**
 * Some helper methods.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class Utils {
  private Utils() {

  }

  private static final ThreadLocal<MessageDigest> MESSAGE_DIGEST_LOCAL =
      new ThreadLocal<MessageDigest>() {

        @Override
        protected MessageDigest initialValue() {
          try {
            return MessageDigest.getInstance("md5");
          } catch (Exception e) {
            throw Reflector.sneakyThrow(e);
          }
        }

      };

  private static final char[] HEX_ARRAY = "0123456789ABCDEF".toCharArray();

  public static String bytesToHex(final byte[] bytes) {
    char[] hexChars = new char[bytes.length * 2];
    for (int j = 0; j < bytes.length; j++) {
      int v = bytes[j] & 0xFF;
      hexChars[j * 2] = HEX_ARRAY[v >>> 4];
      hexChars[j * 2 + 1] = HEX_ARRAY[v & 0x0F];
    }
    return new String(hexChars);
  }

  public static String md5sum(final String s) {
    MessageDigest md = MESSAGE_DIGEST_LOCAL.get();
    md.reset();
    byte[] messageDigest = md.digest(s.getBytes());
    return bytesToHex(messageDigest);
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
