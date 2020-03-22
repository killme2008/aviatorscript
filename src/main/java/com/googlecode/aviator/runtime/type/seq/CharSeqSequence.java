package com.googlecode.aviator.runtime.type.seq;

import java.util.Iterator;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * Sequence for CharSequence.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class CharSeqSequence implements Sequence<Character> {
  private final CharSequence cs;


  public CharSeqSequence(final CharSequence cs) {
    super();
    this.cs = cs;
  }



  @Override
  public Iterator<Character> iterator() {
    return new Iterator<Character>() {
      int i = 0;

      @Override
      public boolean hasNext() {
        return this.i < CharSeqSequence.this.cs.length() - 1;
      }

      @Override
      public Character next() {
        return CharSeqSequence.this.cs.charAt(this.i++);
      }

      @Override
      public void remove() {
        throw new UnsupportedOperationException();
      }

    };
  }

}
