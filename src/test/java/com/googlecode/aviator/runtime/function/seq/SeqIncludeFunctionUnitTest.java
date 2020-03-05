package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.HashSet;
import java.util.Set;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqIncludeFunctionUnitTest {

  @Test
  public void testInclude_Array() {
    Integer[] a = new Integer[3];
    a[0] = 1;
    a[1] = -100;

    SeqIncludeFunction fun = new SeqIncludeFunction();
    final AviatorObject arg1 = AviatorRuntimeJavaType.valueOf(a);
    AviatorObject result = fun.call(null, arg1, AviatorRuntimeJavaType.valueOf(-100));
    assertTrue(result.booleanValue(null));

    // contains null Object
    result = fun.call(null, arg1, AviatorNil.NIL);
    assertTrue(result.booleanValue(null));

    // not match
    result = fun.call(null, arg1, AviatorRuntimeJavaType.valueOf(1000));
    assertFalse(result.booleanValue(null));
  }


  @Test
  public void testInclude_HashSet() {
    Set<Integer> a = new HashSet<Integer>();
    a.add(1);
    a.add(-100);
    a.add(null);

    AviatorObject arg1 = AviatorRuntimeJavaType.valueOf(a);

    SeqIncludeFunction fun = new SeqIncludeFunction();
    AviatorObject result = fun.call(null, arg1, AviatorRuntimeJavaType.valueOf(-100));
    assertTrue(result.booleanValue(null));

    // contains null Object
    result = fun.call(null, arg1, AviatorRuntimeJavaType.valueOf(null));
    assertTrue(result.booleanValue(null));
    result = fun.call(null, arg1, AviatorNil.NIL);
    assertTrue(result.booleanValue(null));

    // not match
    result = fun.call(null, arg1, AviatorRuntimeJavaType.valueOf(1000));
    assertFalse(result.booleanValue(null));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testInclude_String() {
    SeqIncludeFunction fun = new SeqIncludeFunction();
    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"),
        AviatorRuntimeJavaType.valueOf("h"));
  }

}
