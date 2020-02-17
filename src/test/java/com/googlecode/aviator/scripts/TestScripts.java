package com.googlecode.aviator.scripts;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.util.Random;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.utils.Reflector;

public class TestScripts {

  private AviatorEvaluatorInstance instance;

  @Before
  public void setup() throws Exception {
    this.instance = AviatorEvaluator.newInstance();
    this.instance.addStaticFunctions("j", org.junit.Assert.class);
  }

  public Object testScript(final String name, final Object... args) {
    try {
      System.out.println("Testing script " + name);
      Expression exp = this.instance
          .compileScript(TestScripts.class.getResource("/scripts/" + name).getFile(), true);
      return exp.execute(AviatorEvaluator.newEnv(args));
    } catch (Throwable t) {
      Reflector.sneakyThrow(t);
    }
    return null;
  }

  @Test
  public void testIfElse() {
    assertEquals(1, testScript("if_else1.av"));
    assertEquals(2, testScript("if_else2.av"));
    assertEquals(3, testScript("if_else3.av"));
    assertEquals(4, testScript("if_else4.av"));
    assertEquals(5, testScript("if_else5.av"));

    try {
      testScript("if_else6.av");
      fail();
    } catch (ExpressionSyntaxErrorException e) {

    }

    assertEquals("a is less than 10.", testScript("if_elsif1.av", "a", 2));
    assertEquals("a is greater than 10.", testScript("if_elsif1.av", "a", 11));
    assertEquals("a is greater than 100.", testScript("if_elsif1.av", "a", 101));

    assertEquals("a is less than 10.", testScript("if_elsif2.av", "a", 2));
    assertEquals("a is greater than 10.", testScript("if_elsif2.av", "a", 11));
    assertEquals("a is greater than 100.", testScript("if_elsif2.av", "a", 101));

    assertEquals("a is less than 10.", testScript("if_elsif3.av", "a", 8));
    assertEquals("a is greater than 10.", testScript("if_elsif3.av", "a", 12));
    assertEquals("a is greater than 100.", testScript("if_elsif3.av", "a", 112));

    assertEquals(7, testScript("if_else7.av"));
    assertEquals(8, testScript("if_else8.av"));
    assertEquals(null, testScript("if_else9.av"));
  }

  private int testSum(final int n) {
    int sum = 0;
    for (int x = 0; x < n; x++) {
      for (int y = 0; y < n; y++) {
        for (int z = 0; z < n; z++) {
          sum += x + y + z;
        }
      }

    }
    return sum;
  }

  @Test
  public void testForLoop() {
    assertEquals(9, testScript("for1.av"));
    assertEquals(0, testScript("for2.av", "a", 1));
    assertEquals(2, testScript("for2.av", "a", 3));
    assertEquals(99, testScript("for2.av", "a", 100));
    assertEquals(null, testScript("for2.av", "a", -1));
    assertEquals(null, testScript("for2.av", "a", 0));
    assertEquals(45, testScript("for3.av", "a", 10));
    assertEquals(5050, testScript("for3.av", "a", 101));
    assertEquals(testSum(10), testScript("for4.av", "a", 10));
    assertEquals(testSum(50), testScript("for4.av", "a", 50));

    {
      // break statement
      assertEquals(3, testScript("for_break1.av", "a", 5));
      assertEquals(3, testScript("for_break1.av", "a", 101));
      int[] z = (int[]) testScript("for_break2.av", "a", 5);
      assertEquals(4, z[0]);
      assertEquals(3, z[1]);
      assertEquals(12, z[2]);
      z = (int[]) testScript("for_break2.av", "a", 101);
      assertEquals(4, z[0]);
      assertEquals(3, z[1]);
      assertEquals(12, z[2]);

      assertEquals(9, testScript("for_break3.av", "a", 5));
      assertEquals(9, testScript("for_break3.av", "a", 101));

      assertEquals(4, testScript("for_break4.av", "a", 5));
      assertEquals(100, testScript("for_break4.av", "a", 101));
      assertEquals(4, testScript("for_break5.av", "a", 5));
      assertEquals(100, testScript("for_break5.av", "a", 101));
    }

    {
      // continue statement
      assertEquals(5050 - 45, testScript("for_continue1.av", "a", 101));
      assertEquals(10, testScript("for_continue1.av", "a", 11));
      assertEquals(21, testScript("for_continue1.av", "a", 12));
      assertEquals(4, testScript("for_continue2.av", "a", 5));
      assertEquals(100, testScript("for_continue2.av", "a", 101));
      assertEquals(4, testScript("for_continue3.av", "a", 5));
      assertEquals(100, testScript("for_continue3.av", "a", 101));

      assertEquals(108, testScript("for_continue4.av", "a", 101));
      assertEquals(108, testScript("for_continue4.av", "a", 51));
      assertEquals(10, testScript("for_continue4.av", "a", 11));
      assertEquals(21, testScript("for_continue4.av", "a", 12));
    }

    {
      // return statement
      assertEquals(0, testScript("for_return1.av"));
      assertEquals(99, testScript("for_return2.av"));
      assertEquals(56, testScript("for_return3.av"));
      assertEquals(6, testScript("for_return4.av"));
    }
  }

  @Test
  public void testWhileLoop() {
    assertEquals(10, testScript("while1.av"));
    assertEquals(10, testScript("while2.av"));
    assertEquals(10, testScript("while3.av"));
    assertEquals(10, testScript("while4.av"));
    final Object[] tuple = (Object[]) testScript("while5.av", "a", 1, "b", 10);
    assertEquals(2, tuple.length);
    assertEquals(6, tuple[0]);
    assertEquals(5, tuple[1]);
    for (int i = 0; i < 10; i++) {
      final long r = (long) testScript("while6.av");
      System.out.println("rand_int(10)=" + r);
      assertTrue(r >= 5);
    }
  }

  @Test
  public void testLet() {
    assertEquals(1, testScript("let1.av"));
    assertEquals(1, testScript("let2.av"));
    assertEquals(null, testScript("let3.av"));
    assertEquals(9, testScript("let4.av"));
    assertEquals(9, testScript("let5.av"));
  }

  private final Random rand = new Random();

  public int[] genRandomIntArray(final int size) {
    int[] a = new int[size];
    for (int i = 0; i < size; i++) {
      a[i] = this.rand.nextInt();
    }
    return a;
  }

  public void assertSortArray(final int[] a, final int size) {
    assertNotNull(a);
    assertEquals(size, a.length);
    int prev = a[0];
    for (int i = 0; i < a.length; i++) {
      assertTrue(prev <= a[i]);
      prev = a[i];
    }
  }

  @Test
  public void testFunctions() {
    // test qsort
    for (int i = 10; i < 100; i++) {
      int[] a = genRandomIntArray(i);
      assertSortArray((int[]) testScript("qsort.av", "a", a), i);
    }

  }
}
