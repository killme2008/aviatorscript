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
    assertEquals(2.197, (double) this.function.call(env, AviatorNumber.valueOf(9)).getValue(null),
        0.001);
    assertEquals(4.394, (double) this.function.call(env, AviatorNumber.valueOf(81)).getValue(null),
        0.001);
    assertEquals(5.991, (double) this.function.call(env, AviatorNumber.valueOf(400)).getValue(null),
        0.001);

    assertEquals(
        new BigDecimal("5.991").doubleValue(), ((BigDecimal) this.function
            .call(env, AviatorNumber.valueOf(new BigInteger("400"))).getValue(null)).doubleValue(),
        0.001);
    assertEquals(new BigDecimal("6.907755268982137").doubleValue(),
        ((BigDecimal) this.function.call(env, AviatorNumber.valueOf(new BigDecimal("999.99999")))
            .getValue(null)).doubleValue(),
        1e-15);

    env.put("a", 400);
    env.put("b", 9.0);

    assertEquals(5.991,
        ((Number) this.function.call(env, new AviatorJavaType("a")).getValue(null)).doubleValue(),
        0.001);
    assertEquals(2.197,
        ((Number) this.function.call(env, new AviatorJavaType("b")).getValue(null)).doubleValue(),
        0.001);
  }

}
