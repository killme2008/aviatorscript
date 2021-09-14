/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 * <p>
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 * <p>
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 * <p>
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 **/
package com.googlecode.aviator.test.function;

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.assertArrayEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.CompareNotSupportedException;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;

/**
 * Aviator grammar test
 *
 * @author dennis
 */
public class GrammarUnitTest {
  protected AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance(EvalMode.ASM);
  }

  @Test
  public void testMultilineExpressions() {
    assertEquals(7, this.instance.execute("a=3;\r\na+4"));
    try {
      this.instance.execute("4>5 \r\n 6");
      fail();
    } catch (ExpressionSyntaxErrorException e) {

    }
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testIllegalExponent() {
    exec("0**'a'");
  }

  @Test
  public void testExponent() {
    assertEquals(1, exec("0**0"));
    assertEquals(1, exec("1**0"));
    assertEquals(1.0, exec("1.2**0"));
    assertEquals(-9, exec("-3**2"));
    assertEquals(-1.0, exec("-1.2**0"));
    assertEquals(-1, exec("-1**0"));
    assertEquals(new BigDecimal("1"), exec("3M**0"));
    assertEquals(new BigInteger("-1"), exec("-2N**0"));
    assertEquals(1, exec("1 + 4/2**3"));

    assertEquals(1, exec("1 + 4/-2**3"));
    assertEquals(33.0, exec("1 + 4/2**-3"));
    assertEquals(5, exec("1 + 4/2**0"));
    assertEquals(-2.2, exec("1-4**2*5**-1"));
    assertEquals(-2.2, exec("1-(4**2)*(5**-1)"));

    assertEquals(Math.pow(2, 1000), exec("2**1000.0"));
    assertEquals(Math.pow(2, 1000), exec("2.0**1000.0"));
    assertEquals(Math.pow(2, 1000), exec("2.0**1000"));

    assertEquals(new BigDecimal("2.0").pow(1000, RuntimeUtils.getMathContext(null)),
        exec("2.0M**1000"));
    assertEquals(new BigDecimal("2.0").pow(1000, RuntimeUtils.getMathContext(null)),
        exec("2.0M**1000.001"));
    assertEquals(new BigInteger("2").pow(1000), exec("2N**1000.001"));
    assertEquals(new BigInteger("2").pow(1000), exec("2N**1000.001"));

    Expression exp = this.instance.compile("a-b/c**2.0*1000");

    assertEquals(-221.2222222222222, exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3)));
    assertEquals(-221.2222222222222, exp.execute(exp.newEnv("a", 1, "b", -2, "c", -3)));
    assertEquals(322.2222222222222, exec("100-2/-3**2.0*1000"));
    assertEquals(-122.2222222222222, exec("100-2/(-3)**2.0*1000"));
    assertEquals(-122.2222222222222, exp.execute(exp.newEnv("a", 100, "b", 2, "c", -3)));
  }

  @Test
  public void testCompareWithVariableSyntaxSuger() {
    try {
      this.instance.setOption(Options.NIL_WHEN_PROPERTY_NOT_FOUND, false);
      this.instance.execute("data.uid != nil");
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals("Could not find variable data.uid", e.getMessage());
    }
    assertFalse((boolean) this.instance.execute("data.uid != nil",
        AviatorEvaluator.newEnv("data.uid", null)));
    assertTrue((boolean) this.instance.execute("data.uid == nil",
        AviatorEvaluator.newEnv("data.uid", null)));
    {
      this.instance.setOption(Options.NIL_WHEN_PROPERTY_NOT_FOUND, true);
      assertFalse((boolean) this.instance.execute("data.uid != nil"));
      assertTrue((boolean) this.instance.execute("data.uid == nil"));
      assertFalse((boolean) this.instance.execute("data.uid != nil",
          AviatorEvaluator.newEnv("data.uid", null)));
      assertTrue((boolean) this.instance.execute("data.uid == nil",
          AviatorEvaluator.newEnv("data.uid", null)));
      this.instance.setOption(Options.NIL_WHEN_PROPERTY_NOT_FOUND, false);
    }

    assertFalse((boolean) this.instance.execute("data != nil"));
    assertTrue((boolean) this.instance.execute("data == nil"));
  }

  @Test
  public void testEscapeStringInterpolation() {
    this.instance.disableFeature(Feature.StringInterpolation);
    try {
      this.instance.execute("'\\#{name}'");
    } catch (CompileExpressionErrorException e) {

    } finally {
      this.instance.enableFeature(Feature.StringInterpolation);
    }
  }

  @Test
  public void testStringInterpolation() {
    assertEquals("aviator", this.instance.execute("let name='aviator'; '#{name}'"));
    assertEquals("hello,aviator", this.instance.execute("let name='aviator'; 'hello,#{name}'"));
    assertEquals("hello,aviator,great",
        this.instance.execute("let name='aviator'; 'hello,#{name},great'"));
    assertEquals("3", this.instance.execute("'#{1+2}'"));
    assertEquals("good,3", this.instance.execute("'good,#{1+2}'"));

    String name = "aviator";
    Expression exp = this.instance.compile(" a ? '#{name}': 'hello,#{name},great'");
    assertEquals("aviator", exp.execute(exp.newEnv("a", true, "name", name)));
    assertEquals("hello,aviator,great", exp.execute(exp.newEnv("a", false, "name", name)));
    assertEquals("aviator", exp.execute(exp.newEnv("a", true, "name", name)));
    assertEquals("hello,aviator,great", exp.execute(exp.newEnv("a", false, "name", name)));
    assertEquals("cool", exp.execute(exp.newEnv("a", true, "name", "cool")));
    assertEquals("hello,cool,great", exp.execute(exp.newEnv("a", false, "name", "cool")));

    // String Interpolation in string.
    exp = this.instance.compile("'hello,#{a+b+\"good,#{c*d}\"}'");
    assertEquals("hello,3good,12", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("hello,3good,12", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("hello,11good,56", exp.execute(exp.newEnv("a", 5, "b", 6, "c", 7, "d", 8)));

    exp = this.instance.compile("'hello,#{a+b+\"good,#{let c=100; c*d}\"}'");
    assertEquals("hello,3good,400", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("hello,3good,400", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("hello,3good,9900", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 99)));

    exp = this.instance.compile("'#{a+b/c}'");
    assertEquals("1", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("1", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));

    exp = this.instance.compile("'#{a+b/c}' + d");
    assertEquals("14", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("14", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("36", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    assertEquals("36", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    exp = this.instance.compile("d + '#{a+b/c}'");
    assertEquals("41", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("41", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("63", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    assertEquals("63", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));

    exp = this.instance.compile("d + '#{a+b/c}' + a");
    assertEquals("411", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("411", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("633", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    assertEquals("633", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    exp = this.instance.compile("'#{d}#{a+b/c}#{a}'");
    assertEquals("411", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("411", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("633", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    assertEquals("633", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    exp = this.instance.compile("'#{d},a+b/c=#{a+b/c}#{a}'");
    assertEquals("4,a+b/c=11", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("4,a+b/c=11", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("6,a+b/c=33", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    assertEquals("6,a+b/c=33", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    exp = this.instance.compile("'d=#{d},a+b/c=#{a+b/c},a=#{a}'");
    assertEquals("d=4,a+b/c=1,a=1", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("d=4,a+b/c=1,a=1", exp.execute(exp.newEnv("a", 1, "b", 2, "c", 3, "d", 4)));
    assertEquals("d=6,a+b/c=3,a=3", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));
    assertEquals("d=6,a+b/c=3,a=3", exp.execute(exp.newEnv("a", 3, "b", 4, "c", 5, "d", 6)));

    assertEquals("#{name}", this.instance.execute("'\\#{name}'"));
    assertEquals("a#{name}", this.instance.execute("'a\\#{name}'"));
    assertEquals("a#{name}b3", this.instance.execute("'a\\#{name}b#{1+2}'"));
    assertEquals("\\#{name}", this.instance.execute("'\\\\\\#{name}'"));
    assertEquals("#{name3", this.instance.execute("'\\#{name#{1+2}'"));
    try {
      this.instance.execute("'#{1+2'");
      fail();
    } catch (CompileExpressionErrorException e) {
      e.printStackTrace();
    }
    try {
      this.instance.execute("'#{1+*2'");
      fail();
    } catch (CompileExpressionErrorException e) {
      e.printStackTrace();
    }
  }


  private Object exec(final String exp) {
    return this.instance.execute(exp);
  }

  @Test
  public void testNullSequence() {
    assertEquals(0, exec("count(nil)"));
    assertFalse((Boolean) exec("include(nil, 1)"));
    assertNull(exec("map(nil, lambda(x) -> x + 1 end)"));
    assertEquals(10, exec("reduce(nil, lambda(r, x) -> r + x end, 10)"));
    assertNull(exec("sort(nil)"));
    assertNull(exec("filter(nil, lambda(x) -> x > 0 end)"));
    assertTrue((boolean) exec("seq.every(nil, lambda(x) -> x == true end)"));
    assertTrue((boolean) exec("seq.not_any(nil, lambda(x) -> x == true end)"));
    assertNull(exec("seq.some(nil, lambda(x) -> x > 0 end)"));
  }

  @Test
  public void testIfElseVar() {
    String r1 = "result=true;v1='test1';if(!result) {return 'ok';} v2='test2'; result";
    Map<String, Object> env = new HashMap<>();
    Assert.assertTrue((boolean) this.instance.execute(r1, env));
    assertEquals(3, env.size());
    assertEquals(true, env.get("result"));
    assertEquals("test1", env.get("v1"));
    assertEquals("test2", env.get("v2"));
  }

  @Test
  public void testIssue200() {
    this.instance.setOption(Options.NIL_WHEN_PROPERTY_NOT_FOUND, true);
    try {
      assertEquals("nullabc", this.instance.execute("a.b.c + 'abc'"));
    } finally {
      this.instance.setOption(Options.NIL_WHEN_PROPERTY_NOT_FOUND, false);
    }
  }

  @Test
  public void testComments() {
    assertEquals(7, this.instance.execute("a=3;a ## a variable\n+4"));
    assertEquals(15, this.instance.execute("##single comments\r\n   7+8\r\n## another comments"));
  }

  @Test
  public void testVariableSyntaxSugerForFunction() {
    assertEquals(0L, this.instance.execute("m = seq.map(\"func\", lambda()->0 end); m.func()"));
    assertEquals(10000.0d,
        this.instance.execute("m = seq.map('square', lambda(x)-> x*x end); m.square(100.0)"));

    assertEquals(8L, this.instance.execute(
        "m = seq.map('x', lambda(x)-> x end, 'y', lambda(x) -> x+1  end);  m.x(3) + m.y(4)"));
  }

  @Test
  public void testScientificNumber() {
    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    assertEquals(new BigDecimal("1.2e308"), this.instance.compile("1.2e308").execute());
    assertEquals(new BigDecimal("1.2e-308"), this.instance.compile("1.2E-308").execute());
    assertEquals(new BigDecimal("-1.2e-308"), this.instance.compile("-1.2E-308").execute());
    this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, true);
    assertEquals(new BigDecimal("1.2e308"), this.instance.compile("1.2e308").execute());
    assertEquals(new BigDecimal("1.2e-308"), this.instance.compile("1.2E-308").execute());
    assertEquals(new BigDecimal("-1.2e-308"), this.instance.compile("-1.2E-308").execute());

    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
    this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, false);
  }

  @Test
  public void testIssue186() {
    Expression exp = this.instance.compile("1==1");
    assertTrue(exp.getVariableFullNames().isEmpty());
    assertTrue(exp.getVariableNames().isEmpty());

    exp = this.instance.compile("a!=nil");
    List<String> vars = exp.getVariableFullNames();
    assertEquals(1, vars.size());
    assertTrue(vars.contains("a"));
    vars = exp.getVariableNames();
    assertEquals(1, vars.size());
    assertTrue(vars.contains("a"));
  }

  @Test
  public void testIssue77() {
    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    assertTrue((boolean) this.instance.execute("'一二三'=~/.*三/"));
    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
  }

  @Test
  public void testFunctionToString() {
    String expression = "(lambda (x) -> lambda(y) -> lambda(z) -> x + y + z end end end)(appName)";
    HashMap<String, Object> env = new HashMap<>();
    String s = this.instance.execute(expression, env, true).toString();
    assertTrue(s.startsWith("<Lambda,"));
  }

  @Test(expected = ExpressionRuntimeException.class)
  public void testIssue181() {
    Map<String, Object> env = new HashMap<>();
    env.put("a1", 3);
    env.put("a2", 4);
    this.instance.execute("(a1%2=0)&&(a2%2==0)", env);
  }

  @Test
  public void testReturnNullCustomFunction() {
    this.instance.addFunction(new AbstractFunction() {


      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
        try {
          return AviatorLong.valueOf(FunctionUtils.getJavaObject(arg1, env));
        } catch (Exception e) {
          return null;
        }
      }

      @Override
      public String getName() {
        return "SafeCastLong";
      }
    });

    assertNull(this.instance.execute("SafeCastLong('a')"));
  }

  @Test
  public void testIssue162() {
    Object val = this.instance.execute("2017122615550747128008704");
    assertTrue(val instanceof BigInteger);
    assertEquals(new BigInteger("2017122615550747128008704"), val);

    val = this.instance.execute("-2017122615550747128008704");
    assertTrue(val instanceof BigInteger);
    assertEquals(new BigInteger("-2017122615550747128008704"), val);
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIssue177() {
    this.instance.compile("$age >30 ($age < 20)");
  }

  @Test
  public void testIssue175() {
    Map<String, Object> env = new HashMap<>();
    env.put("date1", new Date());
    env.put("date2", null);
    assertTrue((boolean) this.instance.execute("date1>date2", env));
  }

  @Test
  public void testIssue87() {
    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    assertEquals(1L, (long) this.instance.execute("long(1.2)"));
    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
  }

  @Test
  public void testIssue92() {
    HashMap<String, Object> env = new HashMap<>();
    assertEquals("\\", this.instance.execute("'\\\\'", env));
  }

  // 增加测试用例
  @Test
  public void testIntegralIntoDecimal() {
    this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, true);
    assertEquals(1.5D, ((BigDecimal) this.instance.execute("3/2")).doubleValue());
    this.instance.setOption(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL, false);
  }

  /**
   * 类型测试
   */
  @Test
  public void testType() {
    assertTrue(this.instance.execute("1") instanceof Long);
    assertTrue(this.instance.execute("3.2") instanceof Double);
    assertTrue(this.instance.execute(Long.MAX_VALUE + "") instanceof Long);
    assertTrue(this.instance.execute("3.14159265") instanceof Double);

    assertEquals("hello world", this.instance.execute("'hello world'"));
    assertEquals("hello world", this.instance.execute("\"hello world\""));
    assertEquals("hello \" world", this.instance.execute("'hello \" world'"));
    assertEquals("hello 'world'", this.instance.execute("\"hello 'world'\""));
    assertEquals("hello 'world' 'dennis'", this.instance.execute("\"hello 'world' 'dennis'\""));

    assertTrue((Boolean) this.instance.execute("true"));
    assertFalse((Boolean) this.instance.execute("false"));

    assertEquals("\\w+\\d?\\..*", this.instance.execute("/\\w+\\d?\\..*/").toString());
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("_a", 3);
    assertEquals(3, this.instance.execute("_a", env));
    long now = System.currentTimeMillis();
    env.put("currentTime", now);
    assertEquals(now, this.instance.execute("currentTime", env));

  }

  public class Foo {
    int a;

    public Foo() {

    }

    public Foo(final int a) {
      super();
      this.a = a;
    }

    public int getA() {
      return this.a;
    }

    public void setA(final int a) {
      this.a = a;
    }

  }

  public class Bar extends Foo {
    int b;

    public Bar() {

    }

    public Bar(final int a, final int b) {
      super(a);
      this.b = b;
    }

    public int getB() {
      return this.b;
    }

    public void setB(final int b) {
      this.b = b;
    }

  }

  /**
   * 类型转换
   */
  @Test
  public void testTypeConversation() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", new Foo(100));
    env.put("bar", new Bar(99, 999));
    env.put("date", new Date());

    Map<String, Object> map = new HashMap<String, Object>();
    map.put("key", "aviator");
    env.put("tmap", map);
    env.put("bool", Boolean.FALSE);

    // long op long=long
    assertTrue(this.instance.execute("3+3") instanceof Long);
    assertTrue(this.instance.execute("3+3/2") instanceof Long);
    assertTrue(this.instance.execute("foo.a+bar.a", env) instanceof Long);
    assertEquals(1098L, this.instance.execute("bar.a+bar.b", env));

    // double op double=double
    assertTrue(this.instance.execute("3.2+3.3") instanceof Double);
    assertTrue(this.instance.execute("3.01+3.1/2.1") instanceof Double);
    assertTrue(this.instance.execute("3.19+3.1/2.9-1.0/(6.0002*7.7+8.9)") instanceof Double);

    // double + long=double
    assertTrue(this.instance.execute("3+0.02") instanceof Double);
    assertTrue(this.instance.execute("3+0.02-100") instanceof Double);
    assertTrue(this.instance.execute("3+3/2-1/(6*7+8.0)") instanceof Double);
    assertTrue(this.instance.execute("foo.a+3.2-1000", env) instanceof Double);

    // object + string =string
    assertEquals("hello world", this.instance.execute("'hello '+ 'world'"));
    assertEquals("hello aviator", this.instance.execute("'hello '+tmap.key", env));
    assertEquals("true aviator", this.instance.execute("true+' '+tmap.key", env));
    assertEquals("100aviator", this.instance.execute("foo.a+tmap.key", env));
    assertEquals("\\d+hello", this.instance.execute("/\\d+/+'hello'"));
    assertEquals("3.2aviator", this.instance.execute("3.2+tmap.key", env));
    assertEquals("false is false", this.instance.execute("bool+' is false'", env));

  }

  @Test
  public void testNotOperandLimit() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("bool", false);

    assertFalse((Boolean) this.instance.execute("!true"));
    assertTrue((Boolean) this.instance.execute("!bool", env));

    try {
      this.instance.execute("!3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("!3.3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("!/\\d+/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("!'hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }

  }

  @Test
  public void testNegOperandLimit() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("d", -3.3);

    assertEquals(-3L, this.instance.execute("-3"));
    assertEquals(3.3, (Double) this.instance.execute("-d", env), 0.001);

    try {
      this.instance.execute("-true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("-'hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("-/\\d+/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testAddOperandsLimit() {
    Map<String, Object> env = createEnv();

    assertEquals(6, this.instance.execute("1+2+3"));
    assertEquals(2.7, (Double) this.instance.execute("6+d", env), 0.001);
    assertEquals("hello aviator", this.instance.execute("'hello '+s", env));
    assertEquals("-3.3aviator", this.instance.execute("d+s", env));
    assertEquals("trueaviator", this.instance.execute("bool+s", env));
    assertEquals("1aviator3", this.instance.execute("1+s+3", env));

    Foo foo = new Foo(2);
    env.put("foo", foo);
    assertEquals(6, this.instance.execute("1+foo.a+3", env));
    try {
      this.instance.execute("foo+s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("d+bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("1+bool+3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("/\\d+/+100", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

  }

  @Test
  public void testSubOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(3, this.instance.execute("6-1-2", env));
    assertEquals(2.86, (Double) this.instance.execute("6-3.14"), 0.001);
    assertEquals(4.3, (Double) this.instance.execute("1-d", env), 0.001);
    assertEquals(0.0, (Double) this.instance.execute("d-d", env), 0.001);
    assertEquals(1003.3, (Double) this.instance.execute("a-d", env), 0.001);
    doArithOpIllegalOperands("-");
  }

  @Test
  public void testMultOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(300, this.instance.execute("100*3", env));
    assertEquals(18.84, (Double) this.instance.execute("6*3.14"), 0.001);
    assertEquals(-9.9, (Double) this.instance.execute("d*3", env), 0.001);
    assertEquals(10.89, (Double) this.instance.execute("d*d", env), 0.001);
    assertEquals(-3300, (Double) this.instance.execute("a*d", env), 0.001);
    doArithOpIllegalOperands("*");
  }

  @Test
  public void testDivOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(33, this.instance.execute("100/3", env));
    assertEquals(1.9108, (Double) this.instance.execute("6/3.14"), 0.001);
    assertEquals(-1.1, (Double) this.instance.execute("d/3", env), 0.001);
    assertEquals(1.0, (Double) this.instance.execute("d/d", env), 0.001);
    assertEquals(-303.030, (Double) this.instance.execute("a/d", env), 0.001);
    doArithOpIllegalOperands("/");
  }

  @Test
  public void testModOperandsLimit() {
    Map<String, Object> env = createEnv();
    assertEquals(1, this.instance.execute("100%3", env));
    assertEquals(2.86, (Double) this.instance.execute("6%3.14"), 0.001);
    assertEquals(-0.29999, (Double) this.instance.execute("d%3", env), 0.001);
    assertEquals(0.0, (Double) this.instance.execute("d%d", env), 0.001);
    assertEquals(1000 % -3.3, (Double) this.instance.execute("a%d", env), 0.001);
    doArithOpIllegalOperands("%");
  }

  private void doArithOpIllegalOperands(final String op) {
    try {
      this.instance.execute("1" + op + "/\\d+/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("true" + op + "true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("'hello world'" + op + "'hello'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("1" + op + "s");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }

    try {
      this.instance.execute("bool" + op + "d");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("a" + op + "s");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("s" + op + "1000");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("bool" + op + "90.0");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("/hello/" + op + "/good/");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testMatch() {
    assertTrue((Boolean) this.instance.execute("'10'=~/^\\d+$/"));
    assertTrue((Boolean) this.instance.execute("'99'=~/^\\d+$/"));
    assertTrue((Boolean) this.instance.execute("'0'=~/^\\d+$/"));
    assertFalse((Boolean) this.instance.execute("'-3'=~/^\\d+$/"));
    assertFalse((Boolean) this.instance.execute("'-0'=~/^\\d+$/"));
    assertFalse((Boolean) this.instance.execute("'aviator'=~/^\\d+$/"));

    assertTrue((Boolean) this.instance.execute("'10'=~/^[0-9]*[1-9][0-9]*$/"));
    assertTrue((Boolean) this.instance.execute("'1'=~/^[0-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) this.instance.execute("'0'=~/^[0-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) this.instance.execute("'-3'=~/^[0-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) this.instance.execute("'aviator'=~/^[0-9]*[1-9][0-9]*$/"));

    assertTrue((Boolean) this.instance.execute("'-10'=~/^((-\\d+)|(0+))$/"));
    assertTrue((Boolean) this.instance.execute("'-99'=~/^((-\\d+)|(0+))$/"));
    assertFalse((Boolean) this.instance.execute("'99'=~/^((-\\d+)|(0+))$/"));
    assertFalse((Boolean) this.instance.execute("'1'=~/^((-\\d+)|(0+))$/"));
    assertFalse((Boolean) this.instance.execute("'aviator'=~/^((-\\d+)|(0+))$/"));

    // ^-?\d+$
    assertTrue((Boolean) this.instance.execute("'-10'=~/^-?\\d+$/"));
    assertTrue((Boolean) this.instance.execute("'0'=~/^-?\\d+$/"));
    assertTrue((Boolean) this.instance.execute("'10'=~/^-?\\d+$/"));
    assertFalse((Boolean) this.instance.execute("'aviator'=~/^-?\\d+$/"));

    assertTrue((Boolean) this.instance.execute("'abc'=~/^[A-Za-z]+$/"));
    assertTrue((Boolean) this.instance.execute("'ABC'=~/^[A-Za-z]+$/"));
    assertFalse((Boolean) this.instance.execute("'123'=~/^[A-Za-z]+$/"));

    assertFalse((Boolean) this.instance.execute("'abc'=~/^[A-Z]+$/"));
    assertTrue((Boolean) this.instance.execute("'ABC'=~/^[A-Z]+$/"));

    assertTrue((Boolean) this.instance.execute("'abc'=~/^[a-z]+$/"));
    assertFalse((Boolean) this.instance.execute("'ABC'=~/^[a-z]+$/"));

    assertTrue((Boolean) this.instance.execute(
        "'0595-97357355'=~/^((\\+?[0-9]{2,4}\\-[0-9]{3,4}\\-)|([0-9]{3,4}\\-))?([0-9]{7,8})(\\-[0-9]+)?$/"));
    assertTrue((Boolean) this.instance.execute(
        "'0595-3749306-020'=~/^((\\+?[0-9]{2,4}\\-[0-9]{3,4}\\-)|([0-9]{3,4}\\-))?([0-9]{7,8})(\\-[0-9]+)?$/"));
    assertFalse((Boolean) this.instance.execute(
        "'0595-abc'=~/^((\\+?[0-9]{2,4}\\-[0-9]{3,4}\\-)|([0-9]{3,4}\\-))?([0-9]{7,8})(\\-[0-9]+)?$/"));

    assertTrue((Boolean) this.instance.execute("'455729032'=~/^[1-9]*[1-9][0-9]*$/"));
    assertFalse((Boolean) this.instance.execute("'45d729032'=~/^[1-9]*[1-9][0-9]*$/"));
    assertTrue(
        (Boolean) this.instance.execute("'<html>hello</html>'=~/<(.*)>.*<\\/\\1>|<(.*) \\/>/"));
    assertTrue((Boolean) this.instance.execute(
        "'127.0.0.1'=~/^(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])$/"));

    assertFalse((Boolean) this.instance.execute(
        "'127.0.0.'=~/^(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5]).(\\d{1,2}|1\\d\\d|2[0-4]\\d|25[0-5])$/"));
  }

  @Test
  public void testMatchWithFlags() {
    assertTrue((Boolean) this.instance.execute("'abc'=~/(?i)^[a-z]+$/"));
    assertTrue((Boolean) this.instance.execute("'ABC'=~/(?i)^[a-z]+$/"));
  }

  @Test
  public void testMatchWithNull() {
    assertFalse((Boolean) this.instance.execute("nil=~/(?i)^[a-z]+$/"));
    assertFalse((Boolean) this.instance.execute("a=~/(?i)^[a-z]+$/"));
  }


  @Test
  public void testComparePattern() {
    Map<String, Object> env = createEnv();

    assertTrue((Boolean) this.instance.execute("p1==p1", env));
    assertTrue((Boolean) this.instance.execute("p1>=p1", env));
    assertTrue((Boolean) this.instance.execute("p1<=p1", env));
    assertTrue((Boolean) this.instance.execute("p1<p2", env));
    assertTrue((Boolean) this.instance.execute("p2>p1", env));
    assertFalse((Boolean) this.instance.execute("p1>=p2", env));
    assertFalse((Boolean) this.instance.execute("p2<=p1", env));
    assertTrue((Boolean) this.instance.execute("/aviator/>/abc/", env));
    assertFalse((Boolean) this.instance.execute("/aviator/</abc/", env));
    try {
      this.instance.execute("3>/abc/");
      Assert.fail();
    } catch (CompareNotSupportedException e) {
    }
    assertTrue((boolean) this.instance.execute("'abc'!=/abc/"));
    assertFalse((boolean) this.instance.execute("3.999==p1", env));
    assertFalse((boolean) this.instance.execute("false==p1", env));

    try {
      this.instance.execute("p2<=bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
  }

  @Test
  public void testCompareString() {
    Map<String, Object> env = createEnv();
    assertTrue((Boolean) this.instance.execute("'b'>'a'"));
    assertTrue((Boolean) this.instance.execute("'b'>='a'"));
    assertTrue((Boolean) this.instance.execute("'b'!='a'"));
    assertFalse((Boolean) this.instance.execute("'b'<'a'"));
    assertFalse((Boolean) this.instance.execute("'b'<='a'"));

    assertTrue((Boolean) this.instance.execute("s==s", env));
    assertTrue((Boolean) this.instance.execute("s>'abc'", env));
    assertFalse((Boolean) this.instance.execute("s<'abc'", env));
    assertFalse((Boolean) this.instance.execute("s<='abc'", env));
    assertTrue((Boolean) this.instance.execute("s!='abc'", env));
    assertTrue((Boolean) this.instance.execute("s>'abc'", env));
    assertTrue((Boolean) this.instance.execute("s==s", env));
    assertTrue((Boolean) this.instance.execute("nil<'a'", env));
    assertTrue((Boolean) this.instance.execute("'a'>nil", env));
    assertTrue((Boolean) this.instance.execute("'a'> not_exists", env));
    assertTrue((Boolean) this.instance.execute("not_exists<'a'", env));

    try {
      this.instance.execute("bool>s", env);
      Assert.fail();
    } catch (CompareNotSupportedException e) {
    }
    try {
      this.instance.execute("true<'abc'");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("s>bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    assertFalse((boolean) this.instance.execute("100=='hello'"));
    assertTrue((boolean) this.instance.execute("s!=d", env));

    try {
      this.instance.execute("/\\d+/<=s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    assertFalse((boolean) this.instance.execute("'hello'==/[a-zA-Z]/"));
  }

  @Test
  public void testCompareNumber() {
    Map<String, Object> env = createEnv();
    assertTrue((Boolean) this.instance.execute("3>1"));
    assertTrue((Boolean) this.instance.execute("3>=1"));
    assertTrue((Boolean) this.instance.execute("3!=1"));
    assertFalse((Boolean) this.instance.execute("3<1"));
    assertFalse((Boolean) this.instance.execute("3<=1"));
    assertFalse((Boolean) this.instance.execute("3==1"));

    assertTrue((Boolean) this.instance.execute("3>=3"));
    assertTrue((Boolean) this.instance.execute("3<=3"));
    assertTrue((Boolean) this.instance.execute("d<0", env));
    assertTrue((Boolean) this.instance.execute("a>3", env));
    assertTrue((Boolean) this.instance.execute("d>=d", env));
    assertFalse((Boolean) this.instance.execute("d>0", env));
    assertFalse((Boolean) this.instance.execute("d>=0", env));
    assertFalse((Boolean) this.instance.execute("a<3", env));
    assertFalse((Boolean) this.instance.execute("d>=3", env));
    assertFalse((Boolean) this.instance.execute("a<=3", env));

    assertTrue((Boolean) this.instance.execute("a>=a", env));
    assertTrue((Boolean) this.instance.execute("a>d", env));
    assertTrue((Boolean) this.instance.execute("d<a", env));

    try {
      this.instance.execute("bool>3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("true<100");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("d>bool", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    assertFalse((boolean) this.instance.execute("100=='hello'"));
    try {
      this.instance.execute("'good'>a", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("/\\d+/>3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    assertFalse((boolean) this.instance.execute("4.9==/[a-zA-Z]/"));
  }

  @Test
  public void testLogicOr() {
    assertTrue((Boolean) this.instance.execute("true||true"));
    assertTrue((Boolean) this.instance.execute("true||false"));
    assertTrue((Boolean) this.instance.execute("false||true"));
    assertFalse((Boolean) this.instance.execute("false||false"));
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", Boolean.FALSE);
    env.put("b", Boolean.TRUE);
    env.put("s", "hello");
    env.put("c", 3.3);

    assertTrue((Boolean) this.instance.execute("b||b", env));
    assertTrue((Boolean) this.instance.execute("b||a", env));
    assertTrue((Boolean) this.instance.execute("a||b", env));
    assertFalse((Boolean) this.instance.execute("a||a", env));

    try {
      this.instance.execute("3 || true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("false || 3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("c || 3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("false || c", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

    try {
      this.instance.execute("false || s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("c || s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("/test/ || s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    // 测试短路
    assertTrue((Boolean) this.instance.execute("true || s"));
    assertTrue((Boolean) this.instance.execute("true || c"));
    assertTrue((Boolean) this.instance.execute("true || 3"));
    assertTrue((Boolean) this.instance.execute("true || /hello/"));
  }

  @Test
  public void testLogicAnd() {
    assertTrue((Boolean) this.instance.execute("true&&true"));
    assertFalse((Boolean) this.instance.execute("true&&false"));
    assertFalse((Boolean) this.instance.execute("false && true"));
    assertFalse((Boolean) this.instance.execute("false    &&false"));
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", Boolean.FALSE);
    env.put("b", Boolean.TRUE);
    env.put("s", "hello");
    env.put("c", 3.3);

    assertTrue((Boolean) this.instance.execute("b&&  b", env));
    assertFalse((Boolean) this.instance.execute("b    &&a", env));
    assertFalse((Boolean) this.instance.execute("a&&b", env));
    assertFalse((Boolean) this.instance.execute("a    &&    a", env));

    try {
      this.instance.execute("3 && true");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("true && 3");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("c && 3", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("true && c", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

    try {
      this.instance.execute("true && s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("c&& s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    try {
      this.instance.execute("/test/ && s", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {
    }
    // 测试短路
    assertFalse((Boolean) this.instance.execute("false && s"));
    assertFalse((Boolean) this.instance.execute("false &&  c"));
    assertFalse((Boolean) this.instance.execute("false &&  3"));
    assertFalse((Boolean) this.instance.execute("false &&  /hello/"));

  }

  /*
   * 测试三元表达式
   */
  @Test
  public void testTernaryOperator() {
    Map<String, Object> env = new HashMap<String, Object>();
    int i = 0;
    float f = 3.14f;
    String email = "killme2008@gmail.com";
    char ch = 'a';
    boolean t = true;
    env.put("i", i);
    env.put("f", f);
    env.put("email", email);
    env.put("ch", ch);
    env.put("t", t);

    assertEquals(1, this.instance.execute("2>1?1:0"));
    assertEquals(0, this.instance.execute("2<1?1:0"));
    assertEquals(f, (Float) this.instance.execute("false?i:f", env), 0.001);
    assertEquals(i, this.instance.execute("true?i:f", env));
    assertEquals("killme2008",
        this.instance.execute("email=~/([\\w0-9]+)@\\w+\\.\\w+/ ? $1:'unknow'", env));

    assertEquals(f, (Float) this.instance.execute("ch!='a'?i:f", env), 0.001);
    assertEquals(i, this.instance.execute("ch=='a'?i:f", env));
    assertEquals(email, this.instance.execute("t?email:ch", env));

    // 多层嵌套
    assertEquals(ch, this.instance.execute("t? i>0? f:ch : email", env));

    assertEquals(email, this.instance.execute("!t? i>0? f:ch : f>3?email:ch", env));

    // 使用括号
    assertEquals(email, this.instance.execute("!t? (i>0? f:ch) :( f>3?email:ch)", env));
    // 测试错误情况
    try {
      this.instance.execute("f?1:0", env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("'hello'?1:0");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("/test/?1:0");
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      this.instance.execute("!t? (i>0? f:ch) : f>3?email:ch)", env);
      Assert.fail();
    } catch (ExpressionSyntaxErrorException e) {

    }
    try {
      this.instance.execute("!t? (i>0? f:ch : (f>3?email:ch)", env);
      Assert.fail();
    } catch (ExpressionSyntaxErrorException e) {

    }
  }

  /**
   * 测试nil
   */
  @Test
  public void testNilObject() {
    assertTrue((Boolean) this.instance.execute("a==nil"));
    assertTrue((Boolean) this.instance.execute("nil==a"));

    assertFalse((Boolean) this.instance.execute("3==nil"));
    assertTrue((Boolean) this.instance.execute("3!=nil"));

    assertFalse((Boolean) this.instance.execute("3.5==nil"));
    assertTrue((Boolean) this.instance.execute("3.5!=nil"));

    assertFalse((Boolean) this.instance.execute("true==nil"));
    assertTrue((Boolean) this.instance.execute("true!=nil"));

    assertFalse((Boolean) this.instance.execute("false==nil"));
    assertTrue((Boolean) this.instance.execute("false!=nil"));

    assertTrue((Boolean) this.instance.execute("nil==nil"));
    assertFalse((Boolean) this.instance.execute("nil!=nil"));

    assertFalse((Boolean) this.instance.execute("'a'==nil"));
    assertTrue((Boolean) this.instance.execute("'a'!=nil"));

    assertFalse((Boolean) this.instance.execute("/\\d+/==nil"));
    assertTrue((Boolean) this.instance.execute("/\\d+/!=nil"));

    Map<String, Object> env = createEnv();
    assertFalse((Boolean) this.instance.execute("p1==nil", env));
    assertTrue((Boolean) this.instance.execute("p1>nil", env));

    assertFalse((Boolean) this.instance.execute("d==nil", env));
    assertTrue((Boolean) this.instance.execute("d>nil", env));

    assertFalse((Boolean) this.instance.execute("s==nil", env));
    assertTrue((Boolean) this.instance.execute("s>nil", env));

    assertFalse((Boolean) this.instance.execute("bool==nil", env));
    assertTrue((Boolean) this.instance.execute("bool>nil", env));

    assertFalse((Boolean) this.instance.execute("a==nil", env));
    assertTrue((Boolean) this.instance.execute("a>nil", env));

    // null == null
    assertTrue((Boolean) this.instance.execute("a==b"));
    assertFalse((Boolean) this.instance.execute("'s'==a"));
    assertTrue((Boolean) this.instance.execute("'s'>=a"));
    assertTrue((Boolean) this.instance.execute("'s'>a"));
    assertTrue((Boolean) this.instance.execute("bool>unknow", env));

  }

  @Test
  public void testIndex() {
    Map<String, Object> env = new HashMap<String, Object>();
    Integer[] a = new Integer[10];
    for (int i = 0; i < a.length; i++) {
      a[i] = i;
    }
    List<String> list = new ArrayList<String>();
    list.add("hello");
    list.add("world");
    env.put("a", a);
    env.put("list", list);
    final HashSet<Integer> set = new HashSet<Integer>();
    set.add(99);
    env.put("set", set);

    for (int i = 0; i < a.length; i++) {
      assertEquals(a[i], this.instance.execute("a[" + i + "]", env));
      assertEquals(a[i] + i, this.instance.execute("a[" + i + "]+" + i, env));
      assertEquals(a[i] + i, this.instance.execute("a[a[a[" + i + "]-10+10]]+" + i, env));
    }

    assertEquals("hello", this.instance.execute("list[0]", env));
    assertEquals(5, this.instance.execute("string.length(list[0])", env));
    assertEquals("world", this.instance.execute("list[1]", env));
    assertEquals("hello world", this.instance.execute("list[0]+' '+list[1]", env));

    try {
      this.instance.execute("set[0]+' '+set[0]", env);
      fail();
    } catch (ExpressionRuntimeException e) {
      // e.printStackTrace();
    }
  }

  @Test
  public void testArrayIndexAccessWithMethod() {
    assertEquals("a", this.instance.exec("string.split('a,b,c,d',',')[0]"));
    assertEquals("b", this.instance.exec("string.split('a,b,c,d',',')[1]"));
    assertEquals("c", this.instance.exec("string.split('a,b,c,d',',')[2]"));
    assertEquals("d", this.instance.exec("string.split('a,b,c,d',',')[3]"));
    try {
      this.instance.exec("string.split('a,b,c,d',',')[4]");
      fail();
    } catch (ArrayIndexOutOfBoundsException e) {

    }

    assertEquals("a", this.instance.exec("string.split(s,',')[0]", "a,b,c,d"));
    assertEquals("b", this.instance.exec("string.split(s,',')[1]", "a,b,c,d"));
    assertEquals("c", this.instance.exec("string.split(s,',')[2]", "a,b,c,d"));
    assertEquals("d", this.instance.exec("string.split(s,',')[3]", "a,b,c,d"));
  }

  @Test
  public void testArrayIndexAccessWithMethodAndBracket() {
    assertEquals("a", this.instance.exec("(string.split('a,b,c,d',','))[0]"));
    assertEquals("b", this.instance.exec("(string.split('a,b,c,d',','))[1]"));
    assertEquals("c", this.instance.exec("(string.split('a,b,c,d',','))[2]"));
    assertEquals("d", this.instance.exec("(string.split('a,b,c,d',','))[3]"));
    try {
      this.instance.exec("(string.split('a,b,c,d',','))[4]");
      fail();
    } catch (ArrayIndexOutOfBoundsException e) {

    }
    assertEquals("a", this.instance.exec("(string.split(s,','))[0]", "a,b,c,d"));
    assertEquals("b", this.instance.exec("(string.split(s,','))[1]", "a,b,c,d"));
    assertEquals("c", this.instance.exec("(string.split(s,','))[2]", "a,b,c,d"));
    assertEquals("d", this.instance.exec("(string.split(s,','))[3]", "a,b,c,d"));
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalArrayIndexAccessWithMethod1() {
    this.instance.exec("string.split('a,b,c,d',',')[0");
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalArrayIndexAccessWithMethod2() {
    this.instance.exec("string.split('a,b,c,d',','[0]");
  }

  @Test
  public void testFunctionCall() {
    Map<String, Object> env = createEnv();
    assertEquals(10, this.instance.execute(" string.length('hello') + string.length('hello') "));
    assertEquals(3.0, (Double) this.instance
        .execute(" string.length('hello')>5? math.abs(d):math.log10(a) ", env), 0.001);
    assertEquals(3.3, (Double) this.instance
        .execute(" string.length('hello')==5? math.abs(d):math.log10(a) ", env), 0.001);

    assertEquals(3.3,
        (Double) this.instance.execute(
            "string.contains(p1,'A-Z')? d<0? math.abs(d): math.sqrt(a) : string.length(p2)  ", env),
        0.001);

    Date date = new Date(111, 8, 11);
    env.put("date", date);
    assertEquals("2011-09-11", this.instance.execute("date_to_string(date,'yyyy-MM-dd')", env));
    assertEquals(date, this.instance.execute("string_to_date('2011-09-11','yyyy-MM-dd')"));

    assertArrayEquals(new String[] {"a", "b", "c"},
        (String[]) this.instance.execute("string.split('a,b,c',',')"));
    env.put("tmps", new String[] {"hello", "aviator"});
    assertEquals("hello aviator", this.instance.execute("string.join(tmps,\" \")", env));
  }

  private Map<String, Object> createEnv() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("d", -3.3);
    env.put("s", "aviator");
    env.put("bool", true);
    env.put("a", 1000);
    env.put("p1", "[a-z-A-Z]+");
    env.put("p2", "\\d+\\.\\d+");
    return env;
  }

  public class StringGetFromJsonMapFunction extends AbstractFunction {

    @Override
    public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
        final AviatorObject arg2) {
      return new AviatorString(null);
    }

    @Override
    public String getName() {
      return "string.getFromJsonMap";
    }
  }

  @Test
  public void testNullPointerException() {
    this.instance.addFunction(new StringGetFromJsonMapFunction());
    String exp = "string.getFromJsonMap(mapJson, key) == 'abc'";
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("mapJson", "{\"key1\":\"value1\", \"key2\":\"value2\"}");
    env.put("key", "key3");
    assertEquals(false, this.instance.execute(exp, env));
  }

  @Test
  public void testNil() {
    this.instance.addFunction(new StringGetFromJsonMapFunction());
    String exp = "string.getFromJsonMap(mapJson, key) == nil";
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("mapJson", "{\"key1\":\"value1\", \"key2\":\"value2\"}");
    env.put("key", "key3");
    assertEquals(true, this.instance.execute(exp, env));
  }

  @Test
  public void testStatement() {
    assertEquals(4, this.instance.execute("5;3;4"));
    assertEquals(4, this.instance.execute("5;1+2;4"));
    assertEquals(4, this.instance.exec("5;1+2;a", 4));
    assertEquals(4, this.instance.exec("(3+a)*4;1+2;println(b);a", 4, 5));

    Map<String, Object> env = createEnv();
    assertEquals(3.0, (Double) this.instance
        .execute("d+10*a ; string.length('hello')>5? math.abs(d):math.log10(a) ", env), 0.001);
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalStatement1() {
    assertEquals(4, this.instance.execute("5;3+;4"));
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testIllegalStatement2() {
    assertEquals(4,
        this.instance.execute("d+10*a ; string.length('hello')>5? math.abs(d);4:math.log10(a)"));
  }


  @Test
  public void testScientificNotationBiggerSmaller() {
    assertEquals(4E81 / 3E9, this.instance.execute("4E81/3E9"));
    assertEquals(1E9 / 2E101, this.instance.execute("1E9/2E101"));

    assertEquals(4E81 / 3E9, this.instance.exec("a/3E9", 4E81));
    assertEquals(1E9 / 2E101, this.instance.exec("1E9/b", 2E101));
  }

  @Test(expected = ExpressionSyntaxErrorException.class)
  public void test4J() {
    System.out.println(this.instance.execute("4(ss*^^%%$$$$"));
  }

  @Test
  public void testAssignment1() {
    assertEquals(3, this.instance.execute("a=1; a+2"));
    assertEquals(5, this.instance.execute("a=3; b=2; a+b"));
    assertEquals(20.0, this.instance.execute("a=3; b=2; c=a+b; c*4.0"));
    assertEquals(6, this.instance.execute("square = lambda(x) -> x *2 end; square(3)"));
    assertEquals(1, this.instance.execute("a=5;b=4.2 ; c= a > b? 1: 0; c"));
    assertEquals(6, this.instance.execute("add_n = lambda(x) -> lambda(y) -> x + y end end ; "
        + "add_1 = add_n(1) ; " + "add_2 = add_n(2) ;" + " add_1(add_2(3))"));
  }

  @Test
  public void testAssignment() {
    this.instance.setOption(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY, true);
    this.instance.setOption(Options.PUT_CAPTURING_GROUPS_INTO_ENV, true);
    Map<String, Object> env = new HashMap<>();
    env.put("b", 4);
    Object v = this.instance.execute("'hello@4'=~/(.*)@(.*)/ ? a=$2:'not match'", env);
    assertEquals("4", v);
    assertTrue(env.containsKey("$2"));
    assertTrue(env.containsKey("a"));
    assertEquals("4", env.get("a"));
  }

  @Test
  public void testIndexAssignment() {
    assertEquals(5L, this.instance.execute("s=seq.list(1,2); s[0]=3; s[0]+s[1]"));
    int[] a = new int[] {-1, 99, 4};
    Map<String, Object> env = AviatorEvaluator.newEnv("a", a);
    assertEquals(4, this.instance.execute("a[0]=-100; a[2] = 5; reduce(a, +, 0)", env));
    assertEquals(-100, a[0]);
    assertEquals(99, a[1]);
    assertEquals(5, a[2]);
  }
}
