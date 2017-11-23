package com.googlecode.aviator.parser;

import java.util.HashMap;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;


public class AviatorClassLoaderUnitTest {

  @Test
  public void testManyManyExpressions() {
    for (int i = 0; i < 10000; i++) {
      HashMap<String, Object> env = new HashMap<String, Object>();
      env.put("a" + i, i);
      env.put("b" + i, i - 1);
      AviatorEvaluator.execute("a" + i + ">b" + i, env);
    }
  }
}
