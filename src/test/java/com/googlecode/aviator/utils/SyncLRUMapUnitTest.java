package com.googlecode.aviator.utils;

import java.util.concurrent.CyclicBarrier;
import org.junit.Before;
import org.junit.Test;
import static org.junit.Assert.*;


public class SyncLRUMapUnitTest {

  private SyncLRUMap<Integer, Integer> map;


  @Before
  public void setUp() {
    this.map = new SyncLRUMap<Integer, Integer>(10);
  }


  @Test
  public void testPutGet() {
    assertTrue(map.isEmpty());
    assertEquals(0, map.size());
    for (int i = 0; i < 5; i++) {
      assertNull(this.map.get(i));
      this.map.put(i, i);
      assertEquals(i, this.map.get(i));
    }
    assertEquals(5, map.size());
  }


  @Test
  public void testLRU() {
    for (int i = 0; i < 20; i++) {
      assertNull(this.map.get(i));
      this.map.put(i, i);
      assertEquals(i, this.map.get(i));
    }
    assertEquals(10, map.size());
    for (int i = 0; i < 10; i++)
      assertNull(this.map.get(0));
    for (int i = 10; i < 11; i++)
      assertEquals(i, this.map.get(i));
    assertEquals(10, map.size());

  }


  @Test
  public void concurrentTest() throws Exception {
    int threads = 100;
    final CyclicBarrier barrier = new CyclicBarrier(threads + 1);
    for (int i = 0; i < threads; i++) {
      new Thread() {
        public void run() {
          try {
            barrier.await();
            for (int i = 0; i < 10000; i++)
              map.put(i, i);
            for (int i = 0; i < 10000; i++)
              map.get(i);
            barrier.await();
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
      }.start();
    }
    barrier.await();
    barrier.await();
    assertEquals(10, map.size());
    System.out.println(map);
  }
}
