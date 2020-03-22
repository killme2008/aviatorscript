package com.googlecode.aviator.runtime.type.seq;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import com.googlecode.aviator.runtime.type.Collector;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * Sequence for CharSequence.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class CharSeqSequence implements Sequence<String> {
  private final CharSequence cs;


  public CharSeqSequence(final CharSequence cs) {
    super();
    this.cs = cs;
  }

  @Override
  public int hintSize() {
    return this.cs.length();
  }



  @Override
  public Collector newCollector(final int size) {
    final List list = new ArrayList(size > 0 ? size : 10);
    return new Collector() {

      @Override
      public void add(final Object e) {
        list.add(e);
      }

      @Override
      public Object getRawContainer() {
        return list;
      }

    };
  }



  @Override
  public Iterator<String> iterator() {
    return new Iterator<String>() {
      int i = 0;

      @Override
      public boolean hasNext() {
        return this.i < CharSeqSequence.this.cs.length();
      }

      @Override
      public String next() {
        return String.valueOf(CharSeqSequence.this.cs.charAt(this.i++));
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }

    };
  }

}
