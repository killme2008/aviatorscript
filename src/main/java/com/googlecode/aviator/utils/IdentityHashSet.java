package com.googlecode.aviator.utils;



import java.util.AbstractSet;
import java.util.Collection;
import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;

/**
 * Identity hash set based on IdentityHashMap.
 *
 * @author dennis(killme2008@gmail.com)
 *
 * @param <E>
 */
public class IdentityHashSet<E> extends AbstractSet<E> {
  private final Map<E, Boolean> delegate = new IdentityHashMap<E, Boolean>();

  public IdentityHashSet() {}

  public IdentityHashSet(final Collection<E> c) {
    addAll(c);
  }

  @Override
  public int size() {
    return this.delegate.size();
  }

  @Override
  public boolean contains(final Object o) {
    return this.delegate.containsKey(o);
  }

  @Override
  public Iterator<E> iterator() {
    return this.delegate.keySet().iterator();
  }

  @Override
  public boolean add(final E arg0) {
    return this.delegate.put(arg0, Boolean.TRUE) == null;
  }

  @Override
  public boolean remove(final Object o) {
    return this.delegate.remove(o) != null;
  }

  @Override
  public void clear() {
    this.delegate.clear();
  }
}
