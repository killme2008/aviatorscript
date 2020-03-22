package com.googlecode.aviator.runtime.type.seq;

import java.lang.reflect.Array;
import java.util.Iterator;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * Sequence for object array.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ArraySequence implements Sequence<Object> {
  private final Object a;
  private final int len;



  public ArraySequence(final Object a) {
    super();
    this.a = a;
    this.len = Array.getLength(a);
  }



  @Override
  public Iterator<Object> iterator() {
    return new Iterator<Object>() {
      int i = 0;

      @Override
      public boolean hasNext() {
        return this.i < ArraySequence.this.len - 1;
      }

      @Override
      public Object next() {
        return Array.get(ArraySequence.this.a, this.i++);
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }

    };
  }

}
