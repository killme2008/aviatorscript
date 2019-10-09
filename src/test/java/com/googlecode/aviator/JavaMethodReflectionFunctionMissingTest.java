package com.googlecode.aviator;

import static org.junit.Assert.assertEquals;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;
import com.googlecode.aviator.utils.TestUtils;

public class JavaMethodReflectionFunctionMissingTest {

  private AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance();
    this.instance.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
  }

  @Test
  public void testFunctionMissing() {
    assertEquals(2, this.instance.execute("indexOf('hello','l')"));
    assertEquals("heeeo", this.instance.execute("replaceAll('hello','l','e')"));
    assertEquals("hello", this.instance.execute("trim(' hello ')"));
    assertEquals(1, this.instance.execute("signum(3M)"));
    assertEquals("hello world",
        this.instance.execute("execute(__instance__, '\"hello\" + \" world\"')"));
    assertEquals(101L, this.instance.execute("execute(__instance__, '1+100')"));

    TestUtils test = new TestUtils();
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("test", test);
    assertEquals(false, this.instance.execute("isEmpty(test, 1.0)", env));
    assertEquals(true, this.instance.execute("isEmpty(test, '')", env));
  }

  @Test(expected = FunctionNotFoundException.class)
  public void testFunctionNotFound1() {
    assertEquals(1, this.instance.execute("test()"));
  }

  @Test(expected = FunctionNotFoundException.class)
  public void testFunctionNotFound2() {
    assertEquals(1, this.instance.execute("test(nil)"));
  }
}
