package com.googlecode.aviator.runtime.type.seq;

import java.lang.reflect.Array;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import com.googlecode.aviator.runtime.type.Collector;
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



  @Override
  public int hintSize() {
    return this.len;
  }



  @Override
  public Collector newCollector(final int size) {

    if (size <= 0) {
      return new Collector() {
        List<Object> list = new ArrayList<>();

        @Override
        public void add(final Object e) {
          this.list.add(e);
        }

        @Override
        public Object getRawContainer() {
          return this.list.toArray();
        }
      };
    } else {
      return new Collector() {
        Object array = Array.newInstance(Object.class, size);
        int i = 0;

        @Override
        public void add(final Object e) {
          Array.set(this.array, this.i++, e);
        }

        @Override
        public Object getRawContainer() {
          return this.array;
        }
      };
    }
  }



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
        return this.i < ArraySequence.this.len;
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
