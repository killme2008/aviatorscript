package com.googlecode.aviator.runtime.type.seq;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Iterator;
import com.googlecode.aviator.runtime.type.Collector;
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
  public int hintSize() {
    if (this.iterable instanceof Collection) {
      return ((Collection) this.iterable).size();
    }
    return 10;
  }

  @SuppressWarnings("rawtypes")
  @Override
  public Collector newCollector(final int size) {
    Collection coll;
    try {
      coll = (Collection) this.iterable.getClass().newInstance();
    } catch (Throwable t) {
      coll = new ArrayList(size > 0 ? size : 10);
    }
    final Collection container = coll;
    return new Collector() {

      @SuppressWarnings("unchecked")
      @Override
      public void add(final Object e) {
        container.add(e);
      }

      @Override
      public Object getRawContainer() {
        return container;
      }

    };
  }


  @Override
  public Iterator<Object> iterator() {
    return this.iterable.iterator();
  }

}
