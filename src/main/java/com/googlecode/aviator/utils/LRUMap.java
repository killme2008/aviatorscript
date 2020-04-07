package com.googlecode.aviator.utils;

import java.util.LinkedHashMap;


/**
 * LRU map based on LinkedHashMap
 *
 * @author apple
 *
 * @param <K>
 * @param <V>
 */
public class LRUMap<K, V> extends LinkedHashMap<K, V> {
  static final long serialVersionUID = -1L;

  private final int maxCapacity;


  public LRUMap(final int maxCapacity) {
    super(16, 0.75f, true);
    if (maxCapacity <= 0) {
      throw new IllegalArgumentException("Invalid maxCapacity: " + maxCapacity);
    }
    this.maxCapacity = maxCapacity;

  }


  @Override
  public V remove(final Object key) {
    return super.remove(key);
  }


  @Override
  public int size() {
    return super.size();
  }


  @Override
  public V put(final K k, final V v) {
    return super.put(k, v);
  }


  @Override
  public V get(final Object k) {
    return super.get(k);
  }


  @Override
  protected boolean removeEldestEntry(final java.util.Map.Entry<K, V> eldest) {
    return this.size() > this.maxCapacity;
  }

}
