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

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.io.ByteArrayOutputStream;
import java.math.MathContext;
import java.util.Arrays;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ArrayBlockingQueue;
import org.junit.Before;
import org.junit.Test;
import org.springframework.util.StringUtils;
import com.googlecode.aviator.AviatorEvaluatorInstance.StringSegments;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.exception.TimeoutException;
import com.googlecode.aviator.exception.UnsupportedFeatureException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.TestUtils;


public class AviatorEvaluatorInstanceUnitTest {

  protected AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance();
    this.instance.setOption(Options.EVAL_TIMEOUT_MS, 100);
  }

  @Test
  public void testSandboxMode() {
    this.instance.enableSandboxMode();
    try {
      this.instance.execute("new java.util.Date()");
    } catch (UnsupportedFeatureException e) {
      // ignore
    }

    try {
      this.instance.execute("Math.abs(-1)");
    } catch (FunctionNotFoundException e) {
      // ignore
    }

    try {
      this.instance.execute("System.exit(1)");
    } catch (FunctionNotFoundException e) {
      // ignore
    }

    try {
      this.instance.execute("Math.PI");
    } catch (ExpressionRuntimeException e) {
      // ignore
    }
    try {
      this.instance.execute("while(true) {}");
    } catch (ExpressionRuntimeException e) {
      // ignore
    }
    try {
      assertNull(this.instance.execute("__env__"));
    } catch (UnsupportedFeatureException e) {

    }
  }

  @Test
  public void testIssue549() {
    Expression exp = instance.compile(
        "let appkey = get_appkey(\"com.sankuai.aaa.bbb.ccc\"); if appkey != nil{ return appkey.serviceLevel; } return nil;");
    assertTrue(exp.getVariableNames().isEmpty());
    assertTrue(exp.getVariableFullNames().isEmpty());

    exp = instance.compile("if appkey != nil{ return appkey.serviceLevel; } return nil;");
    assertEquals(Arrays.asList("appkey"), exp.getVariableNames());
    assertEquals(Arrays.asList("appkey"), exp.getVariableFullNames());
  }

  @Test
  public void testIssue476() {
    Expression expr = instance.compile("let abc = new String('abc');");
    assertTrue(expr.getVariableFullNames().isEmpty());
    expr = instance.compile("let abc = new String('abc'); abc + x");
    assertEquals(expr.getVariableFullNames(), Arrays.asList("x"));
  }

  @Test(expected = TimeoutException.class)
  public void testEvalTimeout() {
    this.instance.execute("while(true) { }");
  }

  @Test
  public void testEvalTimeoutAndTryAgain() throws Exception {
    Expression exp = this.instance.compile("Thread.sleep(120); a + 1");
    try {
      exp.execute(exp.newEnv("a", 2));
    } catch (TimeoutException e) {
      assertTrue(e.getMessage().contains("Expression execution timed out, exceeded: 100 ms"));
      // ignore
    }
    this.instance.setOption(Options.EVAL_TIMEOUT_MS, 200);
    assertEquals(exp.execute(exp.newEnv("a", 2)), 3);
    Thread.sleep(500);
    assertEquals(exp.execute(exp.newEnv("a", 2)), 3);
  }

  @SuppressWarnings("unchecked")
  @Test
  public void testIssue466() {
    this.instance.addOpFunction(OperatorType.ADD, new AbstractFunction() {

      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        Map<String, Object> test = new HashMap<String, Object>();
        test.put("test", "123");
        return AviatorRuntimeJavaType.valueOf(test);
      }

      @Override
      public String getName() {
        return OperatorType.ADD.getToken();
      }
    });

    Expression compiledExp = this.instance.compile("10000+ 20000 ");
    Object result = compiledExp.execute();
    assertTrue(result instanceof HashMap);
    assertEquals("123", ((Map<String, Object>) result).get("test"));
  }

  @Test
  public void testAliasOperator() {
    this.instance.aliasOperator(OperatorType.AND, "and");
    this.instance.aliasOperator(OperatorType.OR, "或者");
    assertTrue((boolean) this.instance.execute("1==1 && 2==2"));
    assertTrue((boolean) this.instance.execute("1==1 and 2==2"));
    assertTrue((boolean) this.instance.execute("1==3 || 2==2"));
    assertTrue((boolean) this.instance.execute("1==3 或者 2==2"));

    assertTrue((boolean) this.instance.execute("a = (1==3 或者 2==2); a"));

    assertFalse((boolean) this.instance.execute("a = (1==3 或者 2==2) and b == 3; a",
        AviatorEvaluator.newEnv("b", 4)));
    assertTrue((boolean) this.instance.execute("a = (1==3 或者 2==2) and b == 3; a",
        AviatorEvaluator.newEnv("b", 3)));
    try {
      this.instance.aliasOperator(OperatorType.ASSIGNMENT, "assign");
      fail();
    } catch (IllegalArgumentException e) {

    }
  }

  @Test
  public void testClassAllowList() {
    final HashSet<Object> classes = new HashSet<>();
    classes.add(ArrayBlockingQueue.class);
    this.instance.setOption(Options.ALLOWED_CLASS_SET, classes);
    try {
      this.instance.execute("new java.util.Date()");
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals(
          "`class java.util.Date` is not in allowed class set, check Options.ALLOWED_CLASS_SET",
          e.getMessage());
    }

    try {
      this.instance.execute("new String()");
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals(
          "`class java.lang.String` is not in allowed class set, check Options.ALLOWED_CLASS_SET",
          e.getMessage());
    }

    this.instance.execute("try {} catch(IllegalArgumentException e) {}");
    assertTrue(this.instance
        .execute("new java.util.concurrent.ArrayBlockingQueue(10)") instanceof ArrayBlockingQueue);

    // recover default value of option ALLOWED_CLASS_SET after test
    this.instance.getOptionValue(Options.ALLOWED_CLASS_SET).classes = null;
  }

  @Test
  public void testAssignableClazzWhiteList() {
    final HashSet<Object> classes = new HashSet<>();
    classes.add(List.class);
    this.instance.setOption(Options.ASSIGNABLE_ALLOWED_CLASS_SET, classes);

    try {
      this.instance.execute("new java.util.Date()");
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals(
          "`class java.util.Date` is not in allowed class set, check Options.ALLOWED_CLASS_SET",
          e.getMessage());
    }

    List res = (List) this.instance.execute("l = new java.util.ArrayList();seq.add(l,1);l");
    assertEquals(1, res.size());
    assertEquals(Integer.valueOf(1), res.get(0));

    List res2 = (List) this.instance.execute("l = new java.util.LinkedList();seq.add(l,1);l");
    assertEquals(1, res2.size());
    assertEquals(Integer.valueOf(1), res2.get(0));

    // recover default value of option ALLOWED_CLASS_SET after test
    this.instance.getOptionValue(Options.ASSIGNABLE_ALLOWED_CLASS_SET).classes = null;
  }

  @Test
  public void testIssue237() {
    this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, true);
    Map<String, Object> env = AviatorEvaluator.newEnv("a", new Object[] {1, 2, 3});
    assertEquals(1, this.instance.execute("a[0]", env));
    assertEquals(2, this.instance.execute("a[1.1]", env));
    assertEquals(3, this.instance.execute("a[2.0M]", env));
  }

  @Test
  public void testCompileStringSegments() {
    StringSegments segs = this.instance.compileStringSegments("#test");
    assertTrue(segs.isEmpty());

    segs = this.instance.compileStringSegments("#test#.");
    assertTrue(segs.isEmpty());

    segs = this.instance.compileStringSegments("#test#.#");
    assertTrue(segs.isEmpty());


    segs = this.instance.compileStringSegments("#test#{a}");
    assertFalse(segs.isEmpty());
    assertEquals("#testnull", segs.toString(AviatorEvaluator.newEnv(), null));
    assertEquals("#test1", segs.toString(AviatorEvaluator.newEnv("a", 1), null));
  }

  @Test
  public void testEnvProcessor() {
    EnvProcessor processor = new EnvProcessor() {

      @Override
      public void beforeExecute(final Map<String, Object> env, final Expression script) {
        env.put("test", true);
      }

      @Override
      public void afterExecute(final Map<String, Object> env, final Expression script) {
        env.put("test", false);
      }
    };
    assertNull(((Map<String, Object>) this.instance.execute("__env__")).get("test"));
    this.instance.setEnvProcessor(processor);
    assertSame(this.instance.getEnvProcessor(), processor);

    assertEquals(true, this.instance.execute("test"));
    assertEquals(1, this.instance.execute("test ? 1 : 2"));
    assertEquals(false, ((Map<String, Object>) this.instance.execute("__env__")).get("test"));
  }

  @Test
  public void testIssue245() {
    // test EQ
    this.instance.addOpFunction(OperatorType.EQ, new AbstractFunction() {


      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        if (!arg1.getValue(env).equals(arg2.getValue(env))) {
          return AviatorBoolean.TRUE;
        }
        return AviatorBoolean.FALSE;
      }

      @Override
      public String getName() {
        return OperatorType.EQ.getToken();
      }
    });

    assertFalse((boolean) this.instance.execute("1==1"));
    assertTrue((boolean) this.instance.execute("0==1"));
    assertFalse((boolean) this.instance.execute("a==b", AviatorEvaluator.newEnv("a", 1, "b", 1)));
    assertTrue((boolean) this.instance.execute("a==b", AviatorEvaluator.newEnv("a", 1, "b", 0)));

    // test Assignment
    assertEquals(1, this.instance.execute("a=b", AviatorEvaluator.newEnv("a", 1, "b", 1)));
    assertEquals(2, this.instance.execute("a=b", AviatorEvaluator.newEnv("a", 1, "b", 2)));

    this.instance.addOpFunction(OperatorType.ASSIGNMENT, new AbstractFunction() {


      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        int c = arg1.compare(arg2, env);
        return AviatorBoolean.valueOf(c == 0);
      }

      @Override
      public String getName() {
        return OperatorType.ASSIGNMENT.getToken();
      }
    });

    assertTrue((boolean) this.instance.execute("1=1"));
    assertFalse((boolean) this.instance.execute("0=1"));
    assertTrue((boolean) this.instance.execute("a=b", AviatorEvaluator.newEnv("a", 1, "b", 1)));
    assertFalse((boolean) this.instance.execute("a=b", AviatorEvaluator.newEnv("a", 1, "b", 0)));
    assertFalse((boolean) this.instance.execute("a=b", AviatorEvaluator.newEnv("a", 1, "b", 2)));
    assertFalse((boolean) this.instance.execute("a=b",
        AviatorEvaluator.newEnv("a", "hello", "b", "world")));
    assertTrue((boolean) this.instance.execute("a=b",
        AviatorEvaluator.newEnv("a", "hello", "b", "hello")));
  }

  @Test
  public void testMaxLoopCount() {
    this.instance.setOption(Options.MAX_LOOP_COUNT, 3);
    assertEquals(6,
        this.instance.execute("let sum = 0; for x in range(1, 4) { sum = sum + x; } return sum;"));
    try {
      assertEquals(10, this.instance
          .execute("let sum = 0; for x in range(1, 5) { sum = sum + x; } return sum;"));
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals("Overflow max loop count: 3", e.getMessage());
    }

    assertEquals(6, this.instance.execute("reduce(range(1, 4), +, 0)"));
    try {
      assertEquals(10, this.instance.execute("reduce(range(1, 5), +, 0)"));
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals("Overflow max loop count: 3", e.getMessage());
    }
  }

  @Test
  public void testNewInstance() {
    assertNotSame(this.instance, AviatorEvaluator.newInstance());
  }

  @Test
  public void testInternalVars() {
    Map<String, Object> env = new HashMap<>();

    Map<String, Object> theEnv = (Map<String, Object>) this.instance.execute("__env__", env);
    assertTrue(env.isEmpty());
    assertNotNull(theEnv.get(Constants.EXP_VAR));
    assertSame(this.instance, theEnv.get(Constants.INSTANCE_VAR));
    assertSame(theEnv, theEnv.get(Constants.ENV_VAR));
    System.out.println(theEnv);
  }

  @Test
  public void testEnableDisableFeature() {
    assertTrue(
        this.instance.getOptionValue(Options.FEATURE_SET).featureSet.contains(Feature.Assignment));
    this.instance.disableFeature(Feature.Assignment);
    assertFalse(
        this.instance.getOptionValue(Options.FEATURE_SET).featureSet.contains(Feature.Assignment));
    this.instance.enableFeature(Feature.Assignment);
    assertTrue(
        this.instance.getOptionValue(Options.FEATURE_SET).featureSet.contains(Feature.Assignment));
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
    assertEquals(this.instance.getOption(Options.TRACE_EVAL), false);
    assertEquals(this.instance.getOption(Options.FEATURE_SET), Feature.getFullFeatures());
    assertEquals(this.instance.getOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL),
        false);
    assertEquals(this.instance.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.EVAL);
    assertEquals(this.instance.getOption(Options.MATH_CONTEXT), MathContext.DECIMAL128);
  }


  @Test
  public void testSetOptions() {
    try {
      this.instance.setOption(Options.FEATURE_SET, Feature.getCompatibleFeatures());
      assertEquals(this.instance.getOption(Options.FEATURE_SET), Feature.getCompatibleFeatures());
      this.instance.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
      assertEquals(this.instance.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.COMPILE);
    } finally {
      this.instance.setOption(Options.FEATURE_SET, Feature.getFullFeatures());
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

  @Test
  public void evaluatorWithSpecifiedCacheKey() {
    Map<String, Object> env = new HashMap<>();
    env.put("a", "hello");
    env.put("b", " world");
    assertEquals("hello world", this.instance.execute("key", "a + b", env, true));
    final Expression expression = this.instance.getCachedExpressionByKey("key");
    assertNotNull(expression);
    assertEquals("hello world", expression.execute(env));
  }


  @Test(expected = UnsupportedFeatureException.class)
  public void testDisableAssignment() {
    this.instance.disableFeature(Feature.Assignment);
    try {
      this.instance.execute("a=3");
    } finally {
      this.instance.enableFeature(Feature.Assignment);
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
              + "[Aviator TRACE] Result : <String, hecco>\n"
              + "[Aviator TRACE]          <String, hecco> + <String,  world> => <String, hecco world>\n"
              + "[Aviator TRACE] Result : hecco world\n",
          output);
    } finally {
      this.instance.setOption(Options.TRACE_EVAL, false);
      this.instance.setTraceOutputStream(System.out);
    }
  }

  @Test(expected = CompileExpressionErrorException.class)
  public void testValidate1() {
    this.instance.validate("");
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testValidate2() {
    this.instance.validate("s = lambda(x) -> lambda(y) -> x + y end; s(3)(4)");
  }

  @Test
  public void testIssue278() {
    if (this.instance.isFeatureEnabled(Feature.If)) {
      this.instance.validate("if(true) {println('in body')}");
    }
    if (this.instance.isFeatureEnabled(Feature.ForLoop)) {
      this.instance.validate("for x in range(0,3) {println(x)}");
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

  @Test
  public void testIf() {
    assertEquals(3, this.instance.execute("if(true) { 3 } else { 4} "));
  }

  @Test
  public void testReturn() {
    assertEquals("hello", this.instance.execute("return 'hello';"));
  }


  @Test
  public void testFor() {
    assertEquals(45,
        this.instance.execute("sum = 0; for x in range(0, 10) { sum = sum + x ;} return sum;"));
  }

  @Test
  public void testWhile() {
    assertEquals(45, this.instance
        .execute("sum = 0; x = 0; while(x < 10) { sum = sum + x ; x = x + 1;} return sum;"));
  }

  @Test
  public void testLet() {
    assertEquals(1, this.instance.execute("let x =1 ; { let x = 2 ; } return x;"));
  }

  @Test
  public void testFn() {
    assertEquals(6, this.instance.execute("fn square(x) { x * 2 }  square(3)"));
  }
}
