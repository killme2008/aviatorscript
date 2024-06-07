package com.googlecode.aviator.runtime.function.system;

import com.googlecode.aviator.runtime.type.AviatorJavaType;
import org.junit.Test;
import java.util.HashMap;
import java.util.Map;


public class LongFunctionUnitTest {
  @Test(expected = ClassCastException.class)
  public void testCall_WithJavaTypeNullArgument() {
    LongFunction longFunction = new LongFunction();
    AviatorJavaType aviatorJavaType = new AviatorJavaType("var");
    longFunction.call(null, aviatorJavaType);
  }

  @Test(expected = ClassCastException.class)
  public void testCall_WithJavaTypeNotSupportArgument() {
    LongFunction longFunction = new LongFunction();
    AviatorJavaType aviatorJavaType = new AviatorJavaType("var");
    Map<String, Object> env = new HashMap<>();
    env.put("var", true);

    longFunction.call(env, aviatorJavaType);
  }
}
