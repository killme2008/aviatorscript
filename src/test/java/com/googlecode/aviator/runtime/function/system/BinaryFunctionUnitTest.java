package com.googlecode.aviator.runtime.function.system;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;


public class BinaryFunctionUnitTest {

  private Env env;

  @Before
  public void setup() {
    env = TestUtils.getTestEnv();
  }

  @Test
  public void testAddFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.ADD);
    AviatorObject result = fun.call(env, AviatorLong.valueOf(10L), AviatorLong.valueOf(11L));
    assertEquals(21L, (Long) result.getValue(null), 0L);

  }


  @Test
  public void testSubFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.SUB);
    AviatorObject result = fun.call(env, AviatorLong.valueOf(10L), AviatorLong.valueOf(11L));
    assertEquals(-1L, (Long) result.getValue(null), 0L);

  }


  @Test
  public void testMultFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.MULT);
    AviatorObject result = fun.call(env, AviatorLong.valueOf(10L), AviatorLong.valueOf(11L));
    assertEquals(110L, (Long) result.getValue(null), 0L);

  }


  @Test
  public void testDivFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.DIV);
    AviatorObject result = fun.call(env, AviatorLong.valueOf(10L), AviatorLong.valueOf(11L));
    assertEquals(0, (Long) result.getValue(null), 0.00);

  }


  @Test
  public void testModFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.MOD);
    AviatorObject result = fun.call(env, AviatorLong.valueOf(10L), AviatorLong.valueOf(11L));
    assertEquals(10L, (Long) result.getValue(null), 0L);

  }


  @Test
  public void testNegFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.NEG);
    AviatorObject result = fun.call(env, AviatorLong.valueOf(10L));
    assertEquals(-10L, (Long) result.getValue(null), 0L);

  }


  @Test
  public void testNotFunction() {
    BinaryFunction fun = new BinaryFunction(OperatorType.NOT);
    AviatorObject result = fun.call(env, AviatorBoolean.FALSE);
    assertTrue((Boolean) result.getValue(null));

  }
}
