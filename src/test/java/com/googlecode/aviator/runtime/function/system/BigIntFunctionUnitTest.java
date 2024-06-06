package com.googlecode.aviator.runtime.function.system;

import com.googlecode.aviator.runtime.type.AviatorJavaType;
import org.junit.Test;
import java.util.HashMap;
import java.util.Map;


public class BigIntFunctionUnitTest {
  @Test(expected = ClassCastException.class)
  public void testCall_WithJavaTypeNullArgument() {
    BigIntFunction bigIntFunction = new BigIntFunction();
    AviatorJavaType aviatorJavaType = new AviatorJavaType("var");
    bigIntFunction.call(null, aviatorJavaType);
  }

  @Test(expected = ClassCastException.class)
  public void testCall_WithJavaTypeNotSupportArgument() {
    BigIntFunction bigIntFunction = new BigIntFunction();
    AviatorJavaType aviatorJavaType = new AviatorJavaType("var");
    Map<String, Object> env = new HashMap<>();
    env.put("var", true);

    bigIntFunction.call(env, aviatorJavaType);
  }
}
