package com.googlecode.aviator.runtime.function.math;

import static org.junit.Assert.*;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNumber;


public class MathTanFunctionUnitTest extends BaseMathFunctionUnitTestForOneArgument {
  @Before
  public void setUp() {
    this.function = new MathTanFunction();
  }


  @Test
  public void testCall() {
    assertEquals(Math.tan(30), this.function.call(null, AviatorNumber.valueOf(30)).getValue(null));
    assertEquals(Math.tan(1020.999),
        this.function.call(null, AviatorNumber.valueOf(1020.999)).getValue(null));
    assertEquals(Math.tan(400),
        this.function.call(null, AviatorNumber.valueOf(400)).getValue(null));

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", 10000);
    env.put("b", 9.0);

    assertEquals(Math.tan(10000), this.function.call(env, new AviatorJavaType("a")).getValue(null));
    assertEquals(Math.tan(9.0), this.function.call(env, new AviatorJavaType("b")).getValue(null));
  }

}
