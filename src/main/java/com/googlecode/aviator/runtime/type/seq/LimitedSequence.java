package com.googlecode.aviator.runtime.type.seq;

import java.util.Iterator;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.Collector;
import com.googlecode.aviator.runtime.type.Sequence;

public class LimitedSequence<T> implements Sequence<T> {

  private final Sequence<T> seq;
  private final int maxLoopCount;


  public LimitedSequence(final Sequence<T> seq, final int maxLoopCount) {
    super();
    this.seq = seq;
    this.maxLoopCount = maxLoopCount;
  }

  @Override
  public Iterator<T> iterator() {
    final Iterator<T> rawIt = this.seq.iterator();
    return new Iterator<T>() {
      int c = 0;

      @Override
      public boolean hasNext() {
        return rawIt.hasNext();
      }

      @Override
      public T next() {
        if (++this.c >= LimitedSequence.this.maxLoopCount) {
          throw new ExpressionRuntimeException(
              "Overflow max loop count: " + LimitedSequence.this.maxLoopCount);
        }
        return rawIt.next();
      }

      @Override
      public void remove() {
        rawIt.remove();
      }

    };
  }

  @Override
  public Collector newCollector(final int size) {
    // TODO Auto-generated method stub
    return null;
  }

  @Override
  public int hintSize() {
    // TODO Auto-generated method stub
    return 0;
  }

}
