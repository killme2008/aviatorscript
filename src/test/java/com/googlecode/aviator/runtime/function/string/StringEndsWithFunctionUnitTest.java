package com.googlecode.aviator.runtime.function.string;

import static org.junit.Assert.*;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;


public class StringEndsWithFunctionUnitTest extends BaseStringFunctionUnitTest {

  @Before
  public void setUp() {
    this.function = new StringEndsWithFunction();
  }


  @Test
  public void testCall() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("s1", "hello");
    env.put("s2", "llo");
    env.put("ch", 'o');
    env.put("temp", "temp");

    assertTrue((Boolean) this.function
        .call(null, new AviatorString("hello"), new AviatorString("llo")).getValue(null));
    assertFalse((Boolean) this.function
        .call(null, new AviatorString("hello"), new AviatorString("world")).getValue(null));
    assertTrue((Boolean) this.function
        .call(env, new AviatorString("hello"), new AviatorJavaType("s2")).getValue(env));
    assertTrue((Boolean) this.function
        .call(env, new AviatorString("hello"), new AviatorJavaType("ch")).getValue(env));
    assertTrue((Boolean) this.function
        .call(env, new AviatorJavaType("s1"), new AviatorJavaType("s2")).getValue(env));
    assertFalse((Boolean) this.function
        .call(env, new AviatorJavaType("s1"), new AviatorJavaType("temp")).getValue(env));
    assertTrue((Boolean) this.function
        .call(env, new AviatorJavaType("s1"), new AviatorJavaType("ch")).getValue(env));
    assertTrue((Boolean) this.function
        .call(env, new AviatorJavaType("s1"), new AviatorString("llo")).getValue(env));
  }

}
