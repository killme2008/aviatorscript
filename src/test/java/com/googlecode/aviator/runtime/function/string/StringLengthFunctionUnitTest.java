package com.googlecode.aviator.runtime.function.string;

import static org.junit.Assert.*;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.runtime.function.string.StringLengthFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorString;


public class StringLengthFunctionUnitTest {
  private AviatorFunction function;


  @Before
  public void setUp() {
    this.function = new StringLengthFunction();
  }


  @Test
  public void testCall() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("s1", "hello");
    env.put("s2", "hello world");

    assertEquals(5, function.call(null, new AviatorString("hello")).getValue(null));
    assertEquals(11, function.call(null, new AviatorString("hello world")).getValue(null));
    assertEquals(5, function.call(env, new AviatorJavaType("s1")).getValue(null));
    assertEquals(11, function.call(env, new AviatorJavaType("s2")).getValue(null));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testIllegalArguments_null() {
    this.function.call(null);
  }


  @Test(expected = IllegalArgumentException.class)
  public void testIllegalArguments_zero() {
    this.function.call(null);
  }


  @Test(expected = IllegalArgumentException.class)
  public void testIllegalArguments_Three() {
    this.function.call(null, new AviatorString("hello"), new AviatorString("hello"),
        new AviatorString("hello"));
  }


  @Test(expected = ClassCastException.class)
  public void testClassCastError1() {
    this.function.call(null, AviatorBoolean.TRUE);
  }


  @Test(expected = ClassCastException.class)
  public void testClassCastError2() {
    this.function.call(null, AviatorLong.valueOf(3));
  }


  @Test(expected = NullPointerException.class)
  public void testNullPointerException1() {
    this.function.call(null, AviatorNil.NIL);
  }

}
