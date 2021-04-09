package com.googlecode.aviator.utils;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.util.ArrayList;
import java.util.Collection;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Map.Entry;
import java.util.Set;
import java.util.concurrent.ThreadLocalRandom;
import org.junit.Before;
import org.junit.Test;

public class ArrayHashMapTest {
  private Map<Integer, Integer> map;

  @Before
  public void setup() {
    this.map = new ArrayHashMap<>();
  }

  @Test
  public void testRandomKeys() {
    List<String> keys = new ArrayList<String>();
    Map<String, Integer> map = new ArrayHashMap<>();

    for (int i = 0; i < 100; i++) {
      String k = randKey();
      map.put(k, i);
      keys.add(k);
      assertTrue(map.containsKey(k));
      assertEquals(i, (int) map.get(k));
    }

    assertEquals(100, map.size());

    Set<String> kset = map.keySet();
    assertEquals(kset, new HashSet<>(keys));

    for (int i = 0; i < 100; i++) {
      String k = keys.get(i);
      assertTrue(map.containsKey(k));
      assertEquals(i, (int) map.get(k));

      map.remove(k);
      assertFalse(map.containsKey(k));
      assertNull(map.get(k));
    }
    assertEquals(0, map.size());
    for (int i = 0; i < 100; i++) {
      assertNull(map.get(keys.get(i)));
    }


    keys.clear();
    for (int i = 0; i < 10; i++) {
      String k = randKey();
      map.put(k, i);
      keys.add(k);
      assertTrue(map.containsKey(k));
      assertEquals(i, (int) map.get(k));
    }
    assertEquals(10, map.size());
    map.clear();
    assertEquals(0, map.size());

    for (int i = 0; i < 10; i++) {
      assertNull(map.get(keys.get(i)));
    }
  }

  private String randKey() {
    byte[] bs = new byte[16];
    ThreadLocalRandom.current().nextBytes(bs);
    return new String(bs);
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
