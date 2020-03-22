package com.googlecode.aviator.runtime.type.seq;

import java.util.Iterator;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * Sequence for iterable.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class IterableSequence implements Sequence<Object> {
  private final Iterable<Object> iterable;


  public IterableSequence(final Iterable<Object> iterable) {
    super();
    this.iterable = iterable;
  }



  @Override
  public Iterator<Object> iterator() {
    return this.iterable.iterator();
  }

}
