package com.googlecode.aviator.runtime.function.math;

import static org.junit.Assert.assertEquals;
import java.math.BigDecimal;
import java.math.BigInteger;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.utils.Env;


public class MathLogFunctionUnitTest extends BaseMathFunctionUnitTestForOneArgument {
  @Before
  public void setUp() {
    this.function = new MathLogFunction();
  }


  @Test
  public void testCall() {
    Env env = TestUtils.getTestEnv();
    assertEquals(2.197, this.function.call(env, AviatorNumber.valueOf(9)).getValue(null));
    assertEquals(4.394, this.function.call(env, AviatorNumber.valueOf(81)).getValue(null));
    assertEquals(5.991, this.function.call(env, AviatorNumber.valueOf(400)).getValue(null));

    assertEquals(new BigDecimal("5.991"),
        this.function.call(env, AviatorNumber.valueOf(new BigInteger("400"))).getValue(null));
    assertEquals(new BigDecimal("6.907755268982137"),
        this.function.call(env, AviatorNumber.valueOf(new BigDecimal("999.99999"))).getValue(null));

    env.put("a", 400);
    env.put("b", 9.0);

    assertEquals(5.991, this.function.call(env, new AviatorJavaType("a")).getValue(null));
    assertEquals(2.197, this.function.call(env, new AviatorJavaType("b")).getValue(null));
  }

}
