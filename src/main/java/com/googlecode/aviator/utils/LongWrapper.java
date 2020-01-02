package com.googlecode.aviator.utils;

/**
 * Non-threadsafe long
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class LongWrapper {
  private long v;

  public LongWrapper(final long v) {
    this.v = v;
  }

  public final long get() {
    return this.v;
  }

  public final long getAndAdd() {
    return this.v++;
  }
}
