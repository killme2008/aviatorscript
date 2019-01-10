package com.googlecode.aviator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionRuntimeException;

public class LambdaUnitTest {

  @Test
  public void testSimpleLambda() {
    assertEquals(AviatorEvaluator.exec("lambda(a) -> a end(3)"), 3);
    assertEquals(AviatorEvaluator.exec("(lambda(a) -> a end)(3)"), 3);
    assertEquals(AviatorEvaluator.exec("(lambda(a,b) -> a+b end)(3,4)"), 7);
  }

  @Test
  public void testSimpleLambdaWithEnv() {
    assertEquals(AviatorEvaluator.exec("lambda(a) -> a end(x)", 3), 3);
    assertEquals(AviatorEvaluator.exec("(lambda(a) -> a end)(x)", 3), 3);
    assertEquals(AviatorEvaluator.exec("(lambda(a,b) -> a+b end)(x,y)", 3, 4), 7);
  }

  @Test
  public void testLambdaScope() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", 1);
    env.put("x", 2);
    assertEquals(AviatorEvaluator.execute("lambda(a) -> a end(x)", env), 2);
    assertEquals(AviatorEvaluator.execute("lambda(a) -> a+x end(x)", env), 4);
    try {
      assertEquals(AviatorEvaluator.execute("lambda(a) -> a+y end(x)", env), 4);
      fail();
    } catch (ExpressionRuntimeException e) {
      assertTrue(true);
    }
    env.put("y", 99);
    assertEquals(AviatorEvaluator.execute("lambda(a) -> a+y end(x)", env), 101);
  }

  @Test
  public void testLambdaClosure() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("x", 1);
    env.put("y", 2);
    env.put("z", 3);

    AviatorEvaluator.defineFunction("test",
        "lambda(x) -> lambda(y) -> lambda(z) -> x + y + z end end end");
    assertEquals(15, AviatorEvaluator.execute("test(4)(5)(6)", env));

    env.put("a", 4);
    assertEquals(19, AviatorEvaluator.execute("test(4)(5)(6) + a", env));
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testIssue101() {
    String exp = "a=1; b = lambda(x) -> a+ x end ; a=4 ; b(5)";
    AviatorEvaluator.execute(exp); // throw exception
  }

  @Test
  public void testIssue101_assignment() {
    String exp = "a=1; c=a; b = lambda(x) -> a+ x end ; c=4 ; b(5)";
    assertEquals(6, AviatorEvaluator.execute(exp));
  }

  public static class Foo {
    private int a;

    public int getA() {
      return a;
    }

    public void setA(int a) {
      this.a = a;
    }

  }

  @Test
  public void testLambdaWithNestObject() {
    Foo foo = new Foo();
    foo.setA(99);

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", foo);

    assertEquals(AviatorEvaluator.execute("lambda(x) -> x.a end(foo)", env), 99);
  }

  @Test
  public void testSpecialVars() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", "a");
    env.put("b", 3.2);
    Object c = new Object();
    env.put("c", c);

    assertTrue((boolean) AviatorEvaluator.execute("#__env__.a=='a'", env));
    assertTrue((boolean) AviatorEvaluator.execute("#__env__.b==3.2", env));
    assertTrue((boolean) AviatorEvaluator.execute("#__env__.c!=nil", env));

    Map<String, Object> result = (Map<String, Object>) AviatorEvaluator.execute("#__env__", env);

    assertEquals(3, result.size());
    assertEquals("a", result.get("a"));
    assertEquals(3.2, result.get("b"));
    assertSame(c, result.get("c"));

    assertSame(AviatorEvaluator.getInstance(), AviatorEvaluator.execute("#__instance__"));
    AviatorEvaluatorInstance instance = AviatorEvaluator.newInstance();
    assertSame(instance, instance.execute("#__instance__"));
  }

  @Test
  public void testLambdaWithHighOrderFunction() {
    int[] a = new int[100];
    for (int i = 0; i < 100; i++) {
      a[i] = i;
    }
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", a);

    // map
    Object[] ret = (Object[]) AviatorEvaluator.execute("map(a, lambda(x) -> x + 3 end)", env);
    assertEquals(100, ret.length);
    for (int i = 0; i < 100; i++) {
      assertEquals(i + 3, ret[i]);
    }

    // filter
    ret = (Object[]) AviatorEvaluator.execute("filter(a, lambda(x) -> x > 49 end)", env);
    assertEquals(50, ret.length);
    int sum = 0;
    for (int i = 0; i < 50; i++) {
      assertEquals(i + 50, ret[i]);
      sum += (int) ret[i];
    }
    // reduce
    assertEquals(AviatorEvaluator.execute("reduce(a, lambda(r,e) -> r + e end, 0)", env), 4950);

    // chain
    assertEquals(AviatorEvaluator.execute(
        "reduce(filter(a, lambda(x) -> x > 49 end), lambda(r,e) -> r + e end, 0)", env), sum);
  }
}
