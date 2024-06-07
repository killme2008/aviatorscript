package com.googlecode.aviator.runtime.function.system;

import com.googlecode.aviator.runtime.type.AviatorJavaType;
import org.junit.Test;
import java.util.HashMap;
import java.util.Map;


public class DoubleFunctionUnitTest {
  @Test(expected = ClassCastException.class)
  public void testCall_WithJavaTypeNullArgument() {
    DoubleFunction doubleFunction = new DoubleFunction();
    AviatorJavaType aviatorJavaType = new AviatorJavaType("var");
    doubleFunction.call(null, aviatorJavaType);
  }

  @Test(expected = ClassCastException.class)
  public void testCall_WithJavaTypeNotSupportArgument() {
    DoubleFunction doubleFunction = new DoubleFunction();
    AviatorJavaType aviatorJavaType = new AviatorJavaType("var");
    Map<String, Object> env = new HashMap<>();
    env.put("var", true);

    doubleFunction.call(env, aviatorJavaType);
  }
}
