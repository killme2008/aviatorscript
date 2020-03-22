package com.googlecode.aviator.runtime.type.seq;

import java.util.Iterator;
import java.util.Map;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * Sequence for map.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
@SuppressWarnings("rawtypes")
public class MapSequence implements Sequence<Map.Entry> {
  private final Map map;

  public MapSequence(final Map map) {
    super();
    this.map = map;
  }

  @SuppressWarnings("unchecked")
  @Override
  public Iterator<Map.Entry> iterator() {
    return this.map.entrySet().iterator();
  }

}
