package com.googlecode.aviator.runtime.type.seq;

import java.util.Iterator;
import java.util.NoSuchElementException;
import com.googlecode.aviator.runtime.type.Collector;

/**
 * An empty sequence
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class EmptySequence extends AbstractSequence<Object> {

  public static EmptySequence INSTANCE = new EmptySequence();

  public static final class EmptyIterator implements Iterator<Object> {
    public static final EmptyIterator INSTANCE = new EmptyIterator();

    @Override
    public boolean hasNext() {
      return false;
    }

    @Override
    public Object next() {
      throw new NoSuchElementException();
    }

    @Override
    public void remove() {
      throw new UnsupportedOperationException();
    }
  }

  private EmptySequence() {}

  @Override
  public Collector newCollector(final int size) {
    return new ListCollector(size, false);
  }

  @Override
  public int hintSize() {
    return 10;
  }

  @Override
  public Iterator<Object> iterator() {
    return EmptyIterator.INSTANCE;
  }

}
