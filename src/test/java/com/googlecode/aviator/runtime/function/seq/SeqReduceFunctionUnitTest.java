package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.Env;


public class SeqReduceFunctionUnitTest {

  private Env env;

  @Before
  public void setup() {
    this.env = TestUtils.getTestEnv();
  }

  @Test
  public void testReduce_Array() {
    final Map<String, String> m = new HashMap<String, String>();
    m.put("a", "1");
    m.put("b", "2");

    SeqReduceFunction fun = new SeqReduceFunction();
    AviatorObject result = fun.call(this.env, new AviatorRuntimeJavaType(m),
        (AviatorObject) AviatorEvaluator.execute("lambda(x, y) -> x + long(y.value) end"),
        new AviatorRuntimeJavaType(5));
    assertNotNull(result);
    assertEquals(8, result.getValue(null));
  }

  @Test
  public void testReduce_Map() {
    Integer[] a = new Integer[10];
    for (int i = 0; i < a.length; i++) {
      a[i] = i;
    }

    SeqReduceFunction fun = new SeqReduceFunction();
    AviatorObject result = fun.call(this.env, new AviatorRuntimeJavaType(a),
        new AviatorJavaType("+"), new AviatorRuntimeJavaType(0));
    assertNotNull(result);
    assertEquals(45, result.getValue(null));

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testReduce_NullArray() {
    Integer[] a = new Integer[10];
    for (int i = 0; i < a.length; i++) {
      if (i % 2 == 0) {
        a[i] = i;
      }
    }

    SeqReduceFunction fun = new SeqReduceFunction();
    AviatorObject result = fun.call(this.env, new AviatorRuntimeJavaType(a),
        new AviatorJavaType("+"), new AviatorRuntimeJavaType(0));

  }


  @Test
  public void testReduce_Collection() {
    LinkedHashSet<Integer> a = new LinkedHashSet<Integer>();
    for (int i = 0; i < 10; i++) {
      a.add(i);
    }

    SeqReduceFunction fun = new SeqReduceFunction();
    AviatorObject result = fun.call(this.env, new AviatorRuntimeJavaType(a),
        new AviatorJavaType("+"), new AviatorRuntimeJavaType(0));
    assertNotNull(result);
    assertEquals(45, result.getValue(null));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testReduce_IllegalArguments() {
    LinkedHashSet<Integer> a = new LinkedHashSet<Integer>();
    for (int i = 0; i < 10; i++) {
      a.add(i);
    }

    SeqReduceFunction fun = new SeqReduceFunction();
    AviatorObject result =
        fun.call(this.env, new AviatorRuntimeJavaType(a), new AviatorJavaType("+"));
  }


  @Test(expected = IllegalArgumentException.class)
  public void testReduce_String() {

    SeqReduceFunction fun = new SeqReduceFunction();
    AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, new AviatorRuntimeJavaType("hello"),
        new AviatorJavaType("+"), new AviatorRuntimeJavaType(0));
  }

}
