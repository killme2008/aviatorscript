package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.assertEquals;
import java.util.LinkedList;
import java.util.List;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.Env;


public class SeqMapFunctionUnitTest {

  @Test
  public void testMap_Array() {
    final String[] strs = new String[10];
    for (int i = 0; i < strs.length; i++) {
      strs[i] = "hello";
    }
    SeqMapFunction fun = new SeqMapFunction();
    AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, new AviatorRuntimeJavaType(strs),
        new AviatorJavaType("string.length"));
    Object[] array = (Object[]) result.getValue(null);
    for (Object i : array) {
      assertEquals(5, i);
    }
  }


  @Test
  public void testCount_Collection() {
    final List<String> strs = new LinkedList<String>();
    for (int i = 0; i < 10; i++) {
      strs.add("hello");
    }
    SeqMapFunction fun = new SeqMapFunction();
    Env env = TestUtils.getTestEnv();
    AviatorObject result =
        fun.call(env, new AviatorRuntimeJavaType(strs), new AviatorJavaType("string.length"));
    LinkedList array = (LinkedList) result.getValue(env);
    for (Object i : array) {
      assertEquals(5, i);
    }
  }


  @Test(expected = IllegalArgumentException.class)
  public void testMap_String() {
    Env env = TestUtils.getTestEnv();
    SeqMapFunction fun = new SeqMapFunction();

    AviatorObject result =
        fun.call(env, new AviatorRuntimeJavaType("hello"), new AviatorJavaType("string.length"));
  }

}
