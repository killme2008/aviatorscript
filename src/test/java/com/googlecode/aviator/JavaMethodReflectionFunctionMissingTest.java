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

  @Test
  public void benchmark() throws Exception {
    this.instance.addInstanceFunctions("str", String.class);

    int n = 500000;
    benchmarkFunction(n);
    benchmarkFunctionMissing(n);
    benchmarkImportFunction(n);
    long start = System.nanoTime();
    System.out.println("custom function:" + benchmarkFunction(n) + ", cost: "
        + (System.nanoTime() - start) / 1000_000 + " ms.");
    start = System.nanoTime();
    System.out.println("function missing:" + benchmarkFunctionMissing(n) + ", cost: "
        + (System.nanoTime() - start) / 1000_000 + " ms.");
    start = System.nanoTime();
    System.out.println("imported function:" + benchmarkImportFunction(n) + ", cost: "
        + (System.nanoTime() - start) / 1000_000 + " ms.");

  }

  private long benchmarkImportFunction(final int n) {
    Map<String, Object> env = new HashMap<>();
    long result = 0;
    for (int i = 0; i < n; i++) {
      String s = "hello" + i + "world";
      env.put("s", s);
      // by function missing reflection
      result += (long) this.instance.execute("str.indexOf(s, 'w')", env, true);
    }
    return result;
  }

  private long benchmarkFunctionMissing(final int n) {
    Map<String, Object> env = new HashMap<>();
    long result = 0;
    for (int i = 0; i < n; i++) {
      String s = "hello" + i + "world";
      env.put("s", s);
      // by function missing reflection
      result += (long) this.instance.execute("indexOf(s, 'w')", env, true);
    }
    return result;
  }

  private long benchmarkFunction(final int n) {
    Map<String, Object> env = new HashMap<>();
    long result = 0;
    for (int i = 0; i < n; i++) {
      String s = "hello" + i + "world";
      env.put("s", s);
      // by system lib.
      result += (long) this.instance.execute("string.indexOf(s, 'w')", env, true);
    }
    return result;
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
