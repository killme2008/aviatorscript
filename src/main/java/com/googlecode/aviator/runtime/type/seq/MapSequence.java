package com.googlecode.aviator.runtime.type.seq;

import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Map.Entry;
import com.googlecode.aviator.runtime.type.Collector;

/**
 * Sequence for map.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
@SuppressWarnings("rawtypes")
public class MapSequence extends AbstractSequence<Map.Entry> {
  private final Map map;

  public MapSequence(final Map map) {
    super();
    this.map = map;
  }

  @Override
  public int hintSize() {
    return this.map.size();
  }

  @Override
  public Collector newCollector(final int size) {
    if (size > 0) {
      return new ListCollector(size, false);
    } else {
      Map coll;
      try {
        coll = this.map.getClass().newInstance();
      } catch (Throwable t) {
        coll = new HashMap();
      }
      final Map container = coll;
      return new Collector() {

        @SuppressWarnings("unchecked")
        @Override
        public void add(final Object e) {
          Entry entry = (Entry) e;
          container.put(entry.getKey(), entry.getValue());
        }

        @Override
        public Object getRawContainer() {
          return container;
        }

      };
    }
  }

  @SuppressWarnings("unchecked")
  @Override
  public Iterator<Map.Entry> iterator() {
    return this.map.entrySet().iterator();
  }

}
