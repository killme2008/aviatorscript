package com.googlecode.aviator.test.function;

import static org.junit.Assert.assertEquals;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.junit.After;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;

public class OperatorFunctionTest {

  @After
  public void tearDown() {
    AviatorEvaluator.OPS_MAP.clear();
  }

  @Test
  public void testCustomOperatorFunction() {
    AviatorEvaluator.addOpFunction(OperatorType.BIT_AND, new AbstractFunction() {

      @Override
      public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
        return new AviatorString(arg1.getValue(env).toString() + arg2.getValue(env).toString());
      }

      @Override
      public String getName() {
        return "+";
      }
    });

    assertEquals("43", AviatorEvaluator.exec("a&3", 4));
    assertEquals("43", AviatorEvaluator.exec("4&3", 4));
    assertEquals("hello world", AviatorEvaluator.exec("a&' world'", "hello"));
    assertEquals("hello3 world", AviatorEvaluator.exec("a & 3 & ' world'", "hello"));
    Map<String, Object> env = new HashMap<>();
    env.put("list", Arrays.asList(1, 2, 3));
    assertEquals("123", AviatorEvaluator.execute("reduce(list, &, '')", env));
  }
}
