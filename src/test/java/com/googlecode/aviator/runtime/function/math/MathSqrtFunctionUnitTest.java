package com.googlecode.aviator.runtime.function.math;

import static org.junit.Assert.*;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNumber;


public class MathSqrtFunctionUnitTest extends BaseMathFunctionUnitTestForOneArgument {
  @Before
  public void setUp() {
    this.function = new MathSqrtFunction();
  }


  @Test
  public void testCall() {
    assertEquals(3.0, this.function.call(null, AviatorNumber.valueOf(9)).getValue(null));
    assertEquals(9.0, this.function.call(null, AviatorNumber.valueOf(81)).getValue(null));
    assertEquals(20.0, this.function.call(null, AviatorNumber.valueOf(400)).getValue(null));

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", 400);
    env.put("b", 9.0);

    assertEquals(20.0, this.function.call(env, new AviatorJavaType("a")).getValue(null));
    assertEquals(3.0, this.function.call(env, new AviatorJavaType("b")).getValue(null));
  }

}
