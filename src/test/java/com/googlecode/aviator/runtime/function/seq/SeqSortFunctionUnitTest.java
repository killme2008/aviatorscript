package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqSortFunctionUnitTest {

  @Test
  public void testSort_Array() {
    Integer[] a = new Integer[10];
    int index = 0;
    for (int i = 9; i >= 0; i--) {
      a[index++] = i;
    }

    SeqSortFunction fun = new SeqSortFunction();
    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType(a));
    index = 0;
    Integer[] dup = (Integer[]) result.getValue(null);
    assertFalse(Arrays.equals(a, dup));
    for (Integer i : dup) {
      assertEquals(i, index++);
    }
  }


  @Test
  public void testSort_List() {
    List<Integer> a = new ArrayList<Integer>();
    int index = 0;
    for (int i = 9; i >= 0; i--) {
      a.add(i);
    }

    SeqSortFunction fun = new SeqSortFunction();
    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType(a));
    index = 0;
    List<Integer> dup = (List<Integer>) result.getValue(null);
    assertFalse(a.equals(dup));
    System.out.println(a);
    System.out.println(dup);
    for (Integer i : dup) {
      assertEquals(i, index++);
    }
  }


  @Test(expected = IllegalArgumentException.class)
  public void testSort_HashSet() {
    Set<Integer> a = new HashSet<Integer>();
    int index = 0;
    for (int i = 9; i >= 0; i--) {
      a.add(i);
    }

    SeqSortFunction fun = new SeqSortFunction();
    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType(a));
  }
}
