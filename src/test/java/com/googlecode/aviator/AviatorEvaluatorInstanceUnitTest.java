/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.ByteArrayOutputStream;
import java.math.MathContext;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.StringUtils;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;
import com.googlecode.aviator.utils.TestUtils;


public class AviatorEvaluatorInstanceUnitTest {

  private AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance();
  }


  @Test
  public void testNewInstance() {
    assertNotSame(this.instance, AviatorEvaluator.newInstance());
  }

  @Test
  public void testFunctionMissing() {
    this.instance.setFunctionMissing(new FunctionMissing() {

      @Override
      public AviatorObject onFunctionMissing(final String name, final Map<String, Object> env,
          final AviatorObject... args) {
        // Returns the missing function name.
        return new AviatorString(name);
      }
    });

    assertEquals("test", this.instance.execute("test()"));
    assertEquals("abc", this.instance.execute("abc(1,2)"));
  }


  @Test
  public void testAddInstanceMethods() throws Exception {
    assertTrue(this.instance.addInstanceFunctions("ss", String.class).size() > 0);
    assertEquals(2, this.instance.execute("ss.indexOf('hello','l')"));
    assertEquals(3, this.instance.execute("ss.lastIndexOf('hello','l')"));
    assertEquals("hello".compareToIgnoreCase("heLLo"),
        this.instance.execute("ss.compareToIgnoreCase('hello', 'heLLo')"));
    assertFalse((boolean) this.instance.execute("ss.startsWith('hello','l')"));
    assertTrue((boolean) this.instance.execute("ss.endsWith('hello','o')"));

    byte[] bs = (byte[]) this.instance.execute("ss.getBytes(\"hello world\")");
    assertEquals("hello world", new String(bs));
  }

  @Test
  public void testImportFunctions() throws Exception {
    List<String> added = this.instance.importFunctions(String.class);
    assertTrue(added.size() > 0);
    assertEquals(2, this.instance.execute("String.indexOf('hello','l')"));
    assertEquals(3, this.instance.execute("String.lastIndexOf('hello','l')"));
    assertEquals("hello".compareToIgnoreCase("heLLo"),
        this.instance.execute("String.compareToIgnoreCase('hello', 'heLLo')"));
    assertFalse((boolean) this.instance.execute("String.startsWith('hello','l')"));
    assertTrue((boolean) this.instance.execute("String.endsWith('hello','o')"));

    byte[] bs = (byte[]) this.instance.execute("String.getBytes(\"hello world\")");
    assertEquals("hello world", new String(bs));
  }

  @Test
  public void testImportAnnotation() throws Exception {
    List<String> added = this.instance.importFunctions(TestUtils.class);
    assertTestUtilsStaticMethods(added);
  }

  @Test
  public void testAnnotations() throws Exception {
    List<String> added = this.instance.addInstanceFunctions("test", TestUtils.class);
    System.out.println(added);
    assertTrue(added.size() > 1);
    assertTrue(added.contains("test.is_empty"));

    TestUtils t = new TestUtils();
    Map<String, Object> env = new HashMap<>();
    env.put("t", t);
    assertTrue((boolean) this.instance.execute("test.is_empty(t, '')", env));
    assertFalse((boolean) this.instance.execute("test.is_empty(t, t)", env));
    try {
      this.instance.execute("test.is_empty(t, 0)", env);
      fail();
    } catch (Exception e) {
      assertEquals(
          "No matching method is_empty found taking 1 args for class com.googlecode.aviator.utils.TestUtils",
          e.getMessage());
    }
  }

  @Test
  public void testAddStaticFunctions() throws Exception {
    assertTrue(this.instance.addStaticFunctions("str", StringUtils.class).size() > 0);
    assertTrue((boolean) this.instance.execute("str.hasText('3')"));

    Map<String, Object> env = new HashMap<>();
    env.put("a", new StringBuffer("test"));
    env.put("b", new StringBuilder("test"));
    env.put("c", "");
    assertTrue((boolean) this.instance.execute("str.hasText(a)", env));
    assertTrue((boolean) this.instance.execute("str.hasText(b)", env));
    assertFalse((boolean) this.instance.execute("str.hasText(c)", env));


    List<String> added = this.instance.addStaticFunctions("test", TestUtils.class);
    assertTestUtilsStaticMethods(added);
  }


  private void assertTestUtilsStaticMethods(final List<String> added) {
    assertEquals(5, added.size());
    assertTrue(added.contains("test.add"));
    assertTrue(added.contains("test.assertNotNull"));
    assertTrue(added.contains("test.fib"));
    assertTrue(added.contains("test.join"));
    assertTrue(added.contains("test.join2"));

    assertEquals("str", this.instance.execute("test.assertNotNull('abc')"));
    assertEquals("num", this.instance.execute("test.assertNotNull(3)"));
    assertEquals("num", this.instance.execute("test.assertNotNull(3.14)"));
    assertEquals(3L, this.instance.execute("test.add(1,2)"));
    assertEquals(6L, this.instance.execute("test.add(4N,2)"));

    assertEquals(1L, this.instance.execute("test.fib(1)"));
    assertEquals(0L, this.instance.execute("test.fib(0)"));
    assertEquals(55L, this.instance.execute("test.fib(10)"));

    assertEquals("hello,dennis",
        this.instance.execute("test.join(seq.array(java.lang.String, 'hello','dennis'))"));
    assertEquals("hello,dennis",
        this.instance.execute("test.join2(seq.array(java.lang.String, 'hello','dennis'))"));

    assertEquals("hello,dennis",
        this.instance.execute("test.join('hello',seq.array(java.lang.String, 'dennis'))"));

    try {
      this.instance.execute("test.dadd(3.2,2.0)");
      fail();
    } catch (Exception e) {
      assertEquals("Function not found: test.dadd", e.getMessage());
    }

    try {
      this.instance.execute("test.sub(3,2)");
      fail();
    } catch (Exception e) {
      assertEquals("Function not found: test.sub", e.getMessage());
    }

    try {
      this.instance.execute("test.assertNotNull(lambda() -> 3 end)");
      fail();
    } catch (Exception e) {
      assertEquals("No matching method assertNotNull found taking 1 args", e.getMessage());
    }
  }

  @Test
  public void testInstanceCustomFunctions() {
    this.instance.defineFunction("add", "lambda(x,y) -> println(__env__);x + y end");
    assertEquals(7, this.instance.execute("add(3,4)"));
    try {
      AviatorEvaluator.newInstance().exec("add(3,4)");
      fail();
    } catch (ExpressionRuntimeException e) {
      assertTrue(true);
    }
    assertEquals(17, this.instance.execute("add(8,9)"));
    assertEquals(17, this.instance.exec("add(x,9)", 8));
    try {
      AviatorEvaluator.newInstance().exec("add(1,y)", 3);
      fail();
    } catch (ExpressionRuntimeException e) {
      assertTrue(true);
    }
  }

  @Test
  public void testInstanceCustomOperator() {
    AviatorFunction func = (AviatorFunction) this.instance.execute("lambda(x,y) -> x-y end");
    this.instance.addOpFunction(OperatorType.ADD, func);
    assertEquals(-1, this.instance.execute("3+4"));
    assertEquals(7, AviatorEvaluator.execute("3+4"));
    assertEquals(7, AviatorEvaluator.newInstance().execute("3+4"));
  }

  @Test
  public void testInstanceOptions() {
    this.instance.setOption(Options.TRACE_EVAL, true);
    assertTrue((boolean) this.instance.getOption(Options.TRACE_EVAL));
    assertFalse((boolean) AviatorEvaluator.newInstance().getOption(Options.TRACE_EVAL));
  }


  @Test
  public void testCompileWithoutCache() {
    Expression exp1 = this.instance.compile("1+3");
    Expression exp2 = this.instance.compile("1+3");
    assertNotNull(exp1);
    assertNotNull(exp2);
    assertNotSame(exp1, exp2);

    assertEquals(4, exp1.execute(null));
    assertEquals(4, exp2.execute(null));
  }


  @Test
  public void testDefaultOptionValues() {
    assertEquals(this.instance.getOption(Options.TRACE), false);
    assertEquals(this.instance.getOption(Options.TRACE_EVAL), false);
    assertEquals(this.instance.getOption(Options.ALWAYS_USE_DOUBLE_AS_DECIMAL), false);
    assertEquals(this.instance.getOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL),
        false);
    assertEquals(this.instance.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.EVAL);
    assertEquals(this.instance.getOption(Options.MATH_CONTEXT), MathContext.DECIMAL128);
  }


  @Test
  public void testSetOptions() {
    try {
      this.instance.setOption(Options.TRACE, true);
      assertEquals(this.instance.getOption(Options.TRACE), true);
      this.instance.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
      assertEquals(this.instance.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.COMPILE);
    } finally {
      this.instance.setOption(Options.TRACE, false);
      this.instance.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.EVAL);
    }
  }


  @Test
  public void testExec() {
    String exp1 = "b-c+a";
    String exp2 = "2*3.14*(R-r)+b/c";
    String exp3 = "f>d?k:a";
    String exp4 = "map.a > map.b ? list[0][1]: y";
    assertEquals(8, this.instance.exec(exp1, 6, 2, 4));
    assertEquals(104, this.instance.exec(exp1, 99, 3, 8));
    assertEquals(1.14d, this.instance.exec(exp1, 3.14, 3, 1));
    assertEquals(6.28, this.instance.exec(exp2, 4, 3, 0, 100));
    assertEquals(13.304d, this.instance.exec(exp2, 5, 3.2, 4, 2.0));
    assertEquals(3.14d, this.instance.exec(exp3, 2, 1, 3.14, 9));
    assertEquals(9, this.instance.exec(exp3, 1, 2, 3.14, 9));

    Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("a", 100);
    map.put("b", 99);
    int[][] list = new int[1][2];
    list[0] = new int[2];
    list[0][1] = 2000;
    int y = 999;
    assertEquals(2000L, this.instance.exec(exp4, map, list, y));
    map.put("a", 99);
    assertEquals(999L, this.instance.exec(exp4, map, list, y));
  }


  @Test(expected = IllegalArgumentException.class)
  public void testExecIllegalArguments() {
    this.instance.exec("a-b+c", 1, 2);
  }


  @Test
  public void testCompileCache() {
    assertEquals(0, this.instance.getExpressionCacheSize());
    assertFalse(this.instance.isExpressionCached("1+3"));
    Expression exp1 = this.instance.compile("1+3", true);
    Expression exp2 = this.instance.compile("1+3", true);
    assertEquals(1, this.instance.getExpressionCacheSize());
    assertTrue(this.instance.isExpressionCached("1+3"));
    assertNotNull(exp1);
    assertNotNull(exp2);
    assertSame(exp1, exp2);

    assertEquals(4, exp1.execute(null));
    assertEquals(4, exp2.execute(null));
  }


  @Test
  public void testInvalidateCache() {
    Expression exp1 = this.instance.compile("1+3", true);
    Expression exp2 = this.instance.compile("1+3", true);
    assertNotNull(exp1);
    assertNotNull(exp2);
    assertSame(exp1, exp2);
    assertEquals(1, this.instance.getExpressionCacheSize());
    assertTrue(this.instance.isExpressionCached("1+3"));

    this.instance.invalidateCache("1+3");
    assertFalse(this.instance.isExpressionCached("1+3"));
    assertEquals(0, this.instance.getExpressionCacheSize());
    Expression exp3 = this.instance.compile("1+3", true);
    assertNotSame(exp1, exp3);
    assertEquals(1, this.instance.getExpressionCacheSize());
    assertTrue(this.instance.isExpressionCached("1+3"));

    assertEquals(4, exp1.execute(null));
    assertEquals(4, exp2.execute(null));

    assertEquals(4, exp3.execute(null));

  }


  @Test
  public void evaluatorWithoutCache() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", "hello");
    env.put("b", " world");
    assertEquals("hello world", this.instance.execute("a+b", env));
  }


  @Test
  public void evaluatorWithCache() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", "hello");
    env.put("b", " world");
    assertEquals("hello world", this.instance.execute("a+b", env, true));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testDisableAssignment() {
    this.instance.setOption(Options.DISABLE_ASSIGNMENT, true);
    try {
      this.instance.execute("a=3");
    } finally {
      this.instance.setOption(Options.DISABLE_ASSIGNMENT, false);
    }
  }

  @Test
  public void testTraceEval() throws Exception {
    this.instance.setOption(Options.TRACE_EVAL, true);
    ByteArrayOutputStream bs = new ByteArrayOutputStream();
    this.instance.setTraceOutputStream(bs);
    try {
      this.instance.execute("string.replace_all('hello','l','c') + ' world'");
      String output = new String(bs.toByteArray());
      assertEquals(
          "[Aviator TRACE] Func   : string.replace_all(<String, hello>,<String, l>,<String, c>)\n"
              + "[Aviator TRACE]          <String, hecco> + <String,  world> => <String, hecco world>\n"
              + "[Aviator TRACE] Result : hecco world\n",
          output);
    } finally {
      this.instance.setOption(Options.TRACE_EVAL, false);
      this.instance.setTraceOutputStream(System.out);
    }

  }

  @Test(expected = CompileExpressionErrorException.class)
  public void compileBlankExpression1() {
    this.instance.compile("");
  }


  @Test(expected = IllegalArgumentException.class)
  public void testSetNullMathContext() {
    this.instance.setOption(Options.MATH_CONTEXT, null);
  }


  @Test(expected = CompileExpressionErrorException.class)
  public void compileBlankExpression2() {
    this.instance.compile("    ");
  }


  @Test(expected = CompileExpressionErrorException.class)
  public void executeBlankExpression1() {
    this.instance.execute("", null);
  }


  @Test(expected = CompileExpressionErrorException.class)
  public void executeBlankExpression2() {
    this.instance.execute("    ");
  }
}
