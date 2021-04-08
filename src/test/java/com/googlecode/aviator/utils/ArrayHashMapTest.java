package com.googlecode.aviator.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.Collection;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import org.junit.Before;
import org.junit.Test;

public class ArrayHashMapTest {
  private Map<Integer, Integer> map;

  @Before
  public void setup() {
    this.map = new ArrayHashMap<>();
  }

  @Test
  public void testMisc() {
    assertEquals(0, this.map.size());
    assertTrue(this.map.isEmpty());
    for (int i = 0; i < 1000; i++) {
      this.map.put(i, i);
    }

    for (int i = 0; i < 1000; i++) {
      assertEquals(i, (int) this.map.get(i));
    }

    assertEquals(1000, this.map.size());
    assertFalse(this.map.isEmpty());

    Set<Integer> keys = this.map.keySet();
    assertEquals(1000, keys.size());
    for (int i : keys) {
      assertTrue(i >= 0 && i < 1000);
    }

    Collection<Integer> vals = this.map.values();
    assertEquals(1000, vals.size());
    for (int i : vals) {
      assertTrue(i >= 0 && i < 1000);
    }

    Set<Entry<Integer, Integer>> entries = this.map.entrySet();
    assertEquals(1000, entries.size());
    int i = 0;
    for (Map.Entry<Integer, Integer> entry : entries) {
      assertEquals(entry.getKey(), entry.getValue());
      i++;
    }
    assertEquals(1000, i);

    for (i = 0; i < 1000; i++) {
      if (i % 2 == 0) {
        this.map.remove(i);
      }
    }

    for (i = 0; i < 1000; i++) {
      if (i % 2 == 0) {
        assertFalse(this.map.containsKey(i));
        assertFalse(this.map.containsValue(i));
        assertNull(this.map.get(i));
      } else {
        assertTrue(this.map.containsKey(i));
        assertTrue(this.map.containsValue(i));
        assertEquals(i, (int) this.map.get(i));
      }

    }

    assertEquals(500, this.map.size());
    i = 0;
    entries = this.map.entrySet();
    for (Map.Entry<Integer, Integer> entry : entries) {
      assertEquals(entry.getKey(), entry.getValue());
      assertTrue(entry.getKey() % 2 == 1);
      i++;
    }
    assertEquals(i, 500);

    this.map.clear();
    assertEquals(0, this.map.size());
    assertTrue(this.map.isEmpty());

    for (i = 0; i < 1000; i++) {
      assertNull(this.map.get(i));
    }
  }
}
