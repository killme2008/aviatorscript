package com.googlecode.aviator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Tests for null-safe navigation ("a?.b") and the null-coalescing operator ("a ?? b"), in ASM mode.
 * The interpreter mode is covered by the subclass.
 */
public class NullSafeUnitTest {

  protected AviatorEvaluatorInstance instance;

  public static class Node {
    private final String name;
    private final Node next;

    public Node(final String name, final Node next) {
      this.name = name;
      this.next = next;
    }

    public String getName() {
      return this.name;
    }

    public Node getNext() {
      return this.next;
    }
  }

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance();
    this.instance.addFunction(new AbstractFunction() {
      @Override
      public String getName() {
        return "boom";
      }

      @Override
      public AviatorObject call(final Map<String, Object> env) {
        throw new IllegalStateException("boom should not be evaluated");
      }
    });
  }

  private Object exec(final String expression, final Map<String, Object> env) {
    return this.instance.compile(expression, true).execute(env);
  }

  private Object exec(final String expression) {
    return exec(expression, new HashMap<String, Object>());
  }

  private Map<String, Object> nestedMapEnv() {
    Map<String, Object> env = new HashMap<>();
    Map<String, Object> a = new HashMap<>();
    Map<String, Object> b = new HashMap<>();
    b.put("c", 42L);
    a.put("b", b);
    a.put("nb", null);
    env.put("a", a);
    env.put("nullA", null);
    return env;
  }

  @Test
  public void testNullSafeShortCircuitOnNullRoot() {
    Map<String, Object> env = nestedMapEnv();
    assertNull(exec("nullA?.b", env));
    assertNull(exec("nullA?.b.c", env));
    assertNull(exec("nullA?.b?.c", env));
  }

  @Test
  public void testNullSafeNonNullChain() {
    Map<String, Object> env = nestedMapEnv();
    assertEquals(42L, exec("a?.b?.c", env));
    assertEquals(42L, exec("a.b.c", env));
    assertEquals(42L, exec("a?.b.c", env));
  }

  @Test
  public void testNullSafeMidChainNull() {
    Map<String, Object> env = nestedMapEnv();
    // "nb" is null: guarding it short-circuits.
    assertNull(exec("a?.nb?.c", env));
    assertNull(exec("a.nb?.c", env));
  }

  @Test
  public void testUnguardedMidChainNullThrows() {
    Map<String, Object> env = nestedMapEnv();
    // "nb" is null and not guarded, so navigating ".c" must fail.
    try {
      exec("a.nb.c", env);
      fail("should throw");
    } catch (ExpressionRuntimeException e) {
      // expected
    }
    try {
      exec("a?.nb.c", env);
      fail("should throw");
    } catch (ExpressionRuntimeException e) {
      // expected
    }
  }

  @Test
  public void testNullSafeOnJavaBean() {
    Node tail = new Node("tail", null);
    Node head = new Node("head", tail);
    Map<String, Object> env = new HashMap<>();
    env.put("head", head);
    env.put("nullNode", null);

    assertEquals("tail", exec("head?.next?.name", env));
    assertNull(exec("head?.next?.next?.name", env));
    assertNull(exec("nullNode?.next?.name", env));
  }

  @Test
  public void testNullCoalesce() {
    Map<String, Object> env = nestedMapEnv();
    assertEquals(10L, exec("nil ?? 10"));
    assertEquals(3L, exec("3 ?? 10"));
    assertEquals(42L, exec("a?.b?.c ?? 99", env));
    assertEquals(99L, exec("nullA?.b ?? 99", env));
    assertEquals(99L, exec("a?.nb ?? 99", env));
  }

  @Test
  public void testNullCoalesceRightAssociative() {
    assertEquals(7L, exec("nil ?? nil ?? 7"));
    assertEquals(1L, exec("1 ?? nil ?? 7"));
  }

  @Test
  public void testNullCoalesceShortCircuit() {
    // The right operand must not be evaluated when the left operand is not null.
    assertEquals(1L, exec("1 ?? boom()"));
    try {
      exec("nil ?? boom()");
      fail("should throw");
    } catch (IllegalStateException e) {
      assertEquals("boom should not be evaluated", e.getMessage());
    }
  }

  @Test
  public void testTernaryStillWorks() {
    assertEquals(1L, exec("true ? 1 : 2"));
    assertEquals(2L, exec("false ? 1 : 2"));
    // ternary combined with null-coalesce
    assertEquals(5L, exec("(nil ?? 5) > 0 ? nil ?? 5 : 0"));
  }

  @Test
  public void testAssignToNullSafeTargetIsSyntaxError() {
    try {
      this.instance.compile("a?.b = 1");
      fail("should throw");
    } catch (ExpressionSyntaxErrorException e) {
      assertTrue(e.getMessage().contains("null-safe"));
    }
  }

  @Test
  public void testCompileOptimizeLevel() {
    // Exercise the direct code-generator path (no OptimizeCodeGenerator wrapper).
    this.instance.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
    Map<String, Object> env = nestedMapEnv();
    assertNull(exec("nullA?.b?.c", env));
    assertEquals(42L, exec("a?.b?.c", env));
    assertEquals(10L, exec("nullA?.b ?? 10", env));
    assertEquals(1L, exec("1 ?? boom()"));
  }

  @Test
  public void testVariableNamesStripNullSafeMark() {
    Expression exp = this.instance.compile("a?.b?.c + d");
    assertTrue(exp.getVariableFullNames().contains("a.b.c"));
    assertTrue(exp.getVariableNames().contains("a"));
    assertTrue(exp.getVariableNames().contains("d"));
    for (String name : exp.getVariableFullNames()) {
      assertTrue("leaked '?': " + name, !name.contains("?"));
    }
  }
}
