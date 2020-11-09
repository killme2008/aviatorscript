package com.googlecode.aviator.scripts;

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.IOException;
import java.util.Arrays;
import java.util.Random;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.exception.StandardError;
import com.googlecode.aviator.runtime.module.IoModule;
import com.googlecode.aviator.runtime.type.Range;
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
      System.out.println("Testing script " + name + " with args: " + Arrays.toString(args));
      this.instance
          .validate(IoModule.slurp(TestScripts.class.getResource("/scripts/" + name).getFile()));
      Expression exp = this.instance
          .compileScript(TestScripts.class.getResource("/scripts/" + name).getFile(), true);
      return exp.execute(AviatorEvaluator.newEnv(args));
    } catch (Throwable t) {
      Reflector.sneakyThrow(t);
    }
    return null;
  }

  @Test
  public void testOverloadFunction() {
    testScript("overload_function.av");
  }

  @Test
  public void testVariadicFunctions() {
    testScript("variadic_function.av");
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testVariadicFunctionsWrongPosition() {
    testScript("variadic_function2.av");
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testVariadicFunctionsTwoArgs() {
    testScript("variadic_function3.av");
  }

  @Test
  public void testMisc() {
    assertEquals("aviator execute 1 + 2 = 3.", testScript("string_interpolation.av"));
    assertEquals("aviator execute 1 + 2 = 3.", testScript("string_interpolation.av"));
  }

  @Test
  public void testMultilineString() {
    assertEquals("SELECT u.id, u.name\n" + "  FROM USER u\n" + "  WHERE u.id = 1",
        testScript("string.av"));
  }

  @Test
  public void testRange() {
    assertTrue(testScript("range.av") instanceof Range);
  }


  @Test
  public void testStringSeq() {
    assertEquals("hello world", testScript("string_seq.av"));
  }

  @Test
  public void testSeqEntry() {
    assertEquals(3, testScript("seq_entry.av"));
  }

  @Test
  public void testNew() {
    assertEquals(99, testScript("new.av"));
  }

  @Test
  public void testScopes() {
    assertEquals(2, testScript("scope1.av"));
    assertEquals(199, testScript("scope2.av"));
  }

  @Test
  public void testComments() {
    assertEquals(6, testScript("comments.av"));
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
    assertEquals("statement after if", testScript("if_elsif3.av", "a", 112));

    assertEquals(7, testScript("if_else7.av"));
    assertEquals(8, testScript("if_else8.av"));
    assertEquals(null, testScript("if_else9.av"));
    assertEquals(10, testScript("if_else10.av"));

    assertEquals("b is greater than 100.", testScript("if_else_scope.av", "b", 101));
    assertEquals("b is greater than 10.", testScript("if_else_scope.av", "b", 11));
    assertEquals("b is 1.", testScript("if_else_scope.av", "b", 1));
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

    {
      // for statement values
      assertEquals(9, testScript("for5.av"));
    }

    {
      // for null sequence
      assertEquals(10, testScript("for_null.av"));
      assertEquals(13, testScript("for_null.av", "a", new int[] {1, 2}));
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
    testScript("while7.av");
    assertEquals(10, testScript("while8.av"));
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

    // selection sort
    for (int i = 10; i < 100; i++) {
      int[] a = genRandomIntArray(i);
      assertSortArray((int[]) testScript("selection_sort.av", "a", a), i);
    }


    // test fibonacci
    assertEquals(0, testScript("fibonacci.av", "n", 0));
    assertEquals(1, testScript("fibonacci.av", "n", 1));
    assertEquals(1, testScript("fibonacci.av", "n", 2));
    assertEquals(55, testScript("fibonacci.av", "n", 10));
    assertEquals(610, testScript("fibonacci.av", "n", 15));
    assertEquals(6765, testScript("fibonacci.av", "n", 20));
  }

  @Test
  public void testTryCatch() {
    Exception e = (Exception) testScript("try_catch1.av");
    assertTrue(e instanceof StandardError);
    assertEquals("1", ((Throwable) e).getMessage());

    assertEquals(1, testScript("try_catch2.av"));
    e = (Exception) testScript("try_catch3.av");
    assertTrue(e instanceof ClassCastException);
    e = (Exception) testScript("try_catch4.av");
    assertTrue(e instanceof IOException);

    assertEquals(3, testScript("try_catch5.av"));
    assertEquals(1, testScript("try_catch6.av"));
    assertEquals(2, testScript("try_catch7.av"));
  }
}
