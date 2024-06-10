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

import static com.googlecode.aviator.TestUtils.assertArrayEquals;
import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collections;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.atomic.AtomicInteger;
import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.CompareNotSupportedException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.function.system.AssertFunction.AssertFailed;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

public class FunctionTest {

  protected AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance(EvalMode.ASM);
  }

  @Test
  public void testCmp() {
    assertEquals(0, this.instance.execute("cmp(1,1)"));
    assertEquals(1, this.instance.execute("cmp(2,1)"));
    assertEquals(-1, this.instance.execute("cmp(2,3)"));
    assertEquals(0, this.instance.execute("cmp('hello','hello')"));
    assertEquals(-32, this.instance.execute("cmp('hEllo','hello')"));
  }

  @Test(expected = CompareNotSupportedException.class)
  public void testCmpWrongType() {
    assertEquals(0, this.instance.execute("cmp(1,'hello')"));
  }

  @Test
  public void testBigint() {
    assertEquals(BigInteger.ONE, this.instance.execute("bigint(true)"));
    assertEquals(BigInteger.ZERO, this.instance.execute("bigint(false)"));
    assertEquals("bigint", this.instance.execute("type(1N)"));
    assertEquals("bigint", this.instance.execute("type(bigint(1))"));
    assertEquals("bigint", this.instance.execute("type(bigint(1.1))"));
    assertEquals("bigint", this.instance.execute("type(bigint(1M))"));
    assertEquals("bigint", this.instance.execute("type(bigint('1'))"));
    assertEquals("bigint",
        this.instance.execute("type(bigint(a))", AviatorEvaluator.newEnv("a", "100")));
  }

  @Test
  public void testDecimal() {
    assertEquals(BigDecimal.ONE, this.instance.execute("decimal(true)"));
    assertEquals(BigDecimal.ZERO, this.instance.execute("decimal(false)"));
    assertEquals("decimal", this.instance.execute("type(1M)"));
    assertEquals("decimal", this.instance.execute("type(decimal(1))"));
    assertEquals("decimal", this.instance.execute("type(decimal(1.1))"));
    assertEquals("decimal", this.instance.execute("type(decimal(1N))"));
    assertEquals("decimal", this.instance.execute("type(decimal('1'))"));
    assertEquals("decimal",
        this.instance.execute("type(decimal(a))", AviatorEvaluator.newEnv("a", "100")));
  }

  @Test
  public void testIsDefUndef() {
    assertFalse((boolean) this.instance.execute("is_def(x)"));
    assertTrue((boolean) this.instance.execute("let x=1; is_def(x)"));
    assertTrue((boolean) this.instance.execute("let x=1; { is_def(x) }"));
    assertFalse((boolean) this.instance.execute("{ let x=1; }{ is_def(x) }"));
    assertTrue((boolean) this.instance.execute("{ let x=1; }{ let x=2; {is_def(x)} }"));

    // test undef
    assertEquals(1, this.instance.execute("let x=1; undef(x)"));
    assertFalse((boolean) this.instance.execute("let x=1; undef(x); is_def(x)"));
    assertEquals(null, this.instance.execute("let x=1; undef(x); return x;"));
    assertTrue((boolean) this.instance.execute(" let x=1; { undef(x); { is_def(x)} }"));
    assertTrue((boolean) this.instance.execute(" let x=1; { undef(x); } is_def(x)"));

    assertTrue((boolean) this.instance.execute("is_def(x)", AviatorEvaluator.newEnv("x", 1)));
    assertFalse((boolean) this.instance.execute("is_def(y)", AviatorEvaluator.newEnv("x", 1)));
  }

  @Test
  public void testArithmeticExpression() {
    assertEquals(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10,
        this.instance.execute("1+2+3+4+5+6+7+8+9+10"));
    assertEquals(0, this.instance.execute("1+2-3"));
    assertEquals(120, this.instance.execute("1*2*3*4*5"));
    assertEquals(-4, this.instance.execute("1-2-3"));
    assertEquals(2, this.instance.execute("1-(2-3)"));

    assertEquals(50, this.instance.execute("100/2"));
    assertEquals(33, this.instance.execute("100/3"));

    assertEquals(-49, this.instance.execute("1-100/2"));
    assertEquals(51, this.instance.execute("1+100/2"));
    assertEquals(
        6 - (4 / 2 - (4 + 5)) * 2 + 100 / (2 + 1) * 20 - 5 * 5 * 5 + (6 + 1) / (2 - 3 / (1 + 1)),
        this.instance.execute("6-(4/2-(4+5))*2+100/(2+1)*20-5*5*5+(6+1)/(2-3/(1+1))"));

    assertEquals(62.8, this.instance.execute("2*3.14*10"));
    assertEquals(96.3, this.instance.execute("100.3-4"));
    assertEquals(-96.3, this.instance.execute("4-100.3"));
    assertEquals(100.3 / 4 - (4.0 / 2 + 5), this.instance.execute("100.3/4-(4.0/2+5)"));

    assertEquals(1, this.instance.execute("100%3"));
    assertEquals(0, this.instance.execute("1-100%3"));
    assertEquals(100 % 3 * 4.2 + (37 + 95) / (6 * 3 - 18.0),
        (Double) this.instance.execute("100%3*4.2+(37+95)/(6*3-18.0)"), 0.0001);
  }

  @Test
  public void testCaptureFunctionParams1() {
    try {
      this.instance.setOption(Options.CAPTURE_FUNCTION_ARGS, true);

      List<FunctionArgument> params = (List<FunctionArgument>) this.instance
          .execute("f = lambda(a,bc, d) -> __args__ end; f(1,2,100+2)");

      assertEquals(3, params.size());

      System.out.println(params);

      assertEquals(0, params.get(0).getIndex());
      assertEquals("1", params.get(0).getExpression());
      assertEquals(1, params.get(1).getIndex());
      assertEquals("2", params.get(1).getExpression());
      assertEquals(2, params.get(2).getIndex());
      assertEquals("100+2", params.get(2).getExpression());
    } finally {
      this.instance.setOption(Options.CAPTURE_FUNCTION_ARGS, false);
    }
  }

  private static class CustomFunction extends AbstractFunction {

    List<FunctionArgument> args;

    @Override
    public String getName() {
      return "myadd";
    }

    @Override
    public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
        final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4) {
      this.args = FunctionUtils.getFunctionArguments(env);
      return AviatorLong.valueOf(1);
    }

  }


  private static class CustomFunction2 extends AbstractFunction {

    List<FunctionArgument> args;

    @Override
    public String getName() {
      return "myadd2";
    }

    @Override
    public AviatorObject call(final Map<String, Object> env) {
      this.args = FunctionUtils.getFunctionArguments(env);
      return AviatorLong.valueOf(1);
    }

  }

  @Test
  public void testCaptureFunctionParams3() {
    CustomFunction function = new CustomFunction();
    CustomFunction2 function2 = new CustomFunction2();
    try {
      this.instance.setOption(Options.CAPTURE_FUNCTION_ARGS, true);

      this.instance.addFunction(function);
      this.instance.addFunction(function2);

      this.instance.execute("myadd(sum,a,'hello', 4+100) + myadd2()");

      List<FunctionArgument> args = function.args;
      assertNotNull(args);
      assertEquals(4, args.size());

      System.out.println(args);

      assertEquals(0, args.get(0).getIndex());
      assertEquals("sum", args.get(0).getExpression());
      assertEquals(1, args.get(1).getIndex());
      assertEquals("a", args.get(1).getExpression());
      assertEquals(2, args.get(2).getIndex());
      assertEquals("'hello'", args.get(2).getExpression());
      assertEquals(3, args.get(3).getIndex());
      assertEquals("4+100", args.get(3).getExpression());

      assertTrue(function2.args.isEmpty());
    } finally {
      this.instance.setOption(Options.CAPTURE_FUNCTION_ARGS, false);
      this.instance.removeFunction(function);
      this.instance.removeFunction(function2);
    }
  }

  @Test
  public void testCaptureFunctionParams2() {
    CustomFunction function = new CustomFunction();
    try {
      this.instance.setOption(Options.CAPTURE_FUNCTION_ARGS, true);

      this.instance.addFunction(function);

      this.instance.execute("myadd(sum,a,'hello', 4+100)");

      List<FunctionArgument> args = function.args;
      assertNotNull(args);
      assertEquals(4, args.size());

      System.out.println(args);

      assertEquals(0, args.get(0).getIndex());
      assertEquals("sum", args.get(0).getExpression());
      assertEquals(1, args.get(1).getIndex());
      assertEquals("a", args.get(1).getExpression());
      assertEquals(2, args.get(2).getIndex());
      assertEquals("'hello'", args.get(2).getExpression());
      assertEquals(3, args.get(3).getIndex());
      assertEquals("4+100", args.get(3).getExpression());
    } finally {
      this.instance.setOption(Options.CAPTURE_FUNCTION_ARGS, false);
      this.instance.removeFunction(function);
    }
  }


  @Test
  public void testArithmeticExpressionWithVariable() {
    Map<String, Object> env = new HashMap<String, Object>();
    int i = 100;
    float pi = 3.14f;
    double d = -3.9;
    byte b = (byte) 4;
    env.put("i", i);
    env.put("pi", pi);
    env.put("d", d);
    env.put("b", b);
    System.out.println(this.instance.execute("i+pi", env).getClass());

    System.setProperty("aviator.asm.trace", "true");
    assertEquals(-100, this.instance.execute("-i", env));
    assertEquals(-103.4, this.instance.execute("-i-pi", env));
    assertEquals(2 * 3.14 * 10, (Double) this.instance.execute("2*pi*10", env), 0.001);
    assertEquals(3.14 * d * d, (Double) this.instance.execute("pi*d*d", env), 0.001);

    assertEquals((i + pi + d + b) / 4, this.instance.execute("(i+pi+d+b)/4", env));
    assertEquals(200, this.instance.execute("i+100", env));
    assertEquals(0, this.instance.execute("i%4", env));
    assertEquals(i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99, this.instance
        .execute("i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99", env));
  }


  @Test
  public void testOperatorPrecedence() {
    assertEquals(false, this.instance
        .execute("6.7-100>39.6 ? 5==5? 4+5:6-1 : !false ? 5-6>0&& false: 100%3<=5 || 67*40>=100"));
  }


  @Test
  public void testLogicExpression() {
    assertTrue((Boolean) this.instance.execute("3+1==4"));
    assertTrue((Boolean) this.instance.execute("3+1>=4"));
    assertTrue((Boolean) this.instance.execute("3+1<=4"));
    assertFalse((Boolean) this.instance.execute("3+1>4"));
    assertFalse((Boolean) this.instance.execute("3+1<4"));

    assertTrue((Boolean) this.instance.execute("100/2-50==0"));
    assertTrue((Boolean) this.instance.execute("3-(1+2)==0"));
    assertTrue((Boolean) this.instance.execute("3-4/2==1"));

    assertTrue((Boolean) this.instance.execute("3<1 || -3-100<0 && !(100%3>100)"));
    assertTrue((Boolean) this.instance.execute("3>1 || -3-100<0 && !(100%3<100)"));
    assertFalse((Boolean) this.instance.execute("(3>1 || -3-100<0 )&& !(100%3<100)"));
    assertFalse((Boolean) this.instance.execute("3<1 || -3-100<0 && !(100%3<100)"));
  }


  @Test
  public void testLogicExpressionWithVariable() {
    Map<String, Object> env = new HashMap<String, Object>();
    int i = 100;
    float pi = 3.14f;
    double d = -3.9;
    byte b = (byte) 4;
    env.put("i", i);
    env.put("pi", pi);
    env.put("d", d);
    env.put("b", b);
    env.put("bool", false);

    assertEquals(false, this.instance.execute("-i>=0", env));
    assertEquals(true, this.instance.execute("-i-pi<=-100", env));
    assertEquals(true, this.instance.execute("2*pi*10==2 * pi * 10", env));
    assertEquals(true, this.instance.execute("pi*d*d == pi* d *d", env));

    assertEquals((i + pi + d + b) / 4 % 2 > 0, this.instance.execute("(i+pi+d+b)/4%2>0", env));
    assertEquals(true, this.instance.execute("(i+100)%3!=1", env));
    assertEquals(true, this.instance.execute("i%4<=0", env));
    assertEquals(true, this.instance.execute(
        "i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99 ==i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99",
        env));
  }


  @Test
  public void testSystemFunction() {
    this.instance.setOption(Options.TRACE_EVAL, true);
    try {
      // sysdate()
      Object date = this.instance.execute("sysdate()");
      assertNotNull(date);
      assertTrue(date instanceof Date);
      assertEquals(((Date) date).getMinutes(), new Date().getMinutes());

      // now()
      Object now = this.instance.execute("now()");
      assertNotNull(now);
      assertTrue(now instanceof Long);
      assertEquals((Long) now, System.currentTimeMillis(), 10L);

      // rand()
      Object rand1 = this.instance.execute("rand()");
      assertNotNull(rand1);
      assertTrue(rand1 instanceof Double);

      Object rand2 = this.instance.execute("rand(100)");
      assertTrue(rand2 instanceof Long);
      assertTrue((Long) rand2 < 100);

      Object rand3 = this.instance.execute("rand()");
      assertFalse(rand3.equals(rand1));
    } finally {
      this.instance.setOption(Options.TRACE_EVAL, false);
    }

  }


  @Test
  public void testSeqFunction() {
    Map<String, Object> env = new HashMap<String, Object>();
    Integer[] a = new Integer[10];
    for (int i = 0; i < a.length; i++) {
      a[i] = 9 - i;
    }
    List<String> list = new ArrayList<String>();
    list.add("hello");
    list.add("world");
    env.put("a", a);
    env.put("list", list);
    final HashSet<Boolean> set = new HashSet<Boolean>();
    set.add(true);
    set.add(false);
    env.put("set", set);

    assertEquals(10, this.instance.execute("count(a)", env));
    assertEquals(2, this.instance.execute("count(list)", env));
    assertEquals(2, this.instance.execute("count(set)", env));

    assertTrue((Boolean) this.instance.execute("include(set,true)", env));
    assertTrue((Boolean) this.instance.execute("include(set,false)", env));
    assertFalse((Boolean) this.instance.execute("include(set,'hello')", env));
    assertFalse((Boolean) this.instance.execute("include(set,10)", env));

    for (int i = 0; i < a.length; i++) {
      assertTrue((Boolean) this.instance.execute("include(a,9-" + i + ")", env));
    }

    assertEquals(45, this.instance.execute("reduce(a,+,0)", env));
    assertEquals(0, this.instance.execute("reduce(a,*,1)", env));
    try {
      assertEquals(0, this.instance.execute("reduce(a,/,0)", env));
      fail();
    } catch (ArithmeticException e) {
      // ignore
    }
    assertEquals(-45, this.instance.execute("reduce(a,-,0)", env));

    assertEquals(5, this.instance.execute("count(filter(a,seq.gt(4)))", env));
    assertEquals(4, this.instance.execute("count(filter(a,seq.lt(4)))", env));
    assertEquals(5, this.instance.execute("count(filter(a,seq.le(4)))", env));
    assertEquals(1, this.instance.execute("count(filter(a,seq.eq(4)))", env));
    assertEquals(0, this.instance.execute("count(filter(a,seq.gt(9)))", env));
    assertEquals(0, this.instance.execute("count(filter(a,seq.nil()))", env));
    assertEquals(10, this.instance.execute("count(filter(a,seq.exists()))", env));

    // seq.and and seq.or
    assertEquals(3, this.instance.execute("count(filter(a, seq.and(seq.lt(8), seq.gt(4))))", env));
    assertEquals(4, this.instance.execute("count(filter(a, seq.and(seq.lt(8), seq.ge(4))))", env));
    assertEquals(5, this.instance.execute("count(filter(a, seq.and(seq.le(8), seq.ge(4))))", env));
    assertEquals(5, this.instance.execute("count(filter(a, seq.or(seq.gt(8), seq.lt(4))))", env));
    assertEquals(6, this.instance.execute("count(filter(a, seq.or(seq.gt(8), seq.le(4))))", env));
    assertEquals(7, this.instance.execute("count(filter(a, seq.or(seq.ge(8), seq.le(4))))", env));

    assertEquals(1, this.instance.execute("count(filter(set,seq.true()))", env));
    assertTrue((Boolean) this.instance.execute("include(filter(set,seq.true()),true)", env));
    assertFalse((Boolean) this.instance.execute("include(filter(set,seq.true()),false)", env));
    assertEquals(1, this.instance.execute("count(filter(set,seq.eq(true)))", env));
    assertEquals(1, this.instance.execute("count(filter(set,seq.false()))", env));
    assertFalse((Boolean) this.instance.execute("include(filter(set,seq.false()),true)", env));
    assertTrue((Boolean) this.instance.execute("include(filter(set,seq.false()),false)", env));
    assertEquals(0, this.instance.execute("count(filter(set,seq.nil()))", env));
    assertEquals(2, this.instance.execute("count(filter(set,seq.exists()))", env));

    assertEquals(list, this.instance.execute("sort(list)", env));
    assertNotSame(list, this.instance.execute("sort(list)", env));
    try {
      this.instance.execute("sort(set)", env);
      fail();
    } catch (IllegalArgumentException e) {
      // ignore
    }

    assertEquals(9, a[0]);
    assertFalse(Arrays.equals(a, (Object[]) this.instance.execute("sort(a)", env)));
    assertEquals(9, a[0]);
    Arrays.sort(a);
    assertEquals(0, a[0]);
    assertTrue(Arrays.equals(a, (Object[]) this.instance.execute("sort(a)", env)));

    assertEquals(2, this.instance.execute("count(map(list,string.length))", env));
    assertTrue((Boolean) this.instance.execute("include(map(list,string.length),5)", env));

    assertTrue((Boolean) this.instance.execute("seq.every(tuple(true,true,true), identity)"));
    assertFalse((Boolean) this.instance.execute("seq.every(tuple(true,false,true), identity)"));
    assertTrue((Boolean) this.instance.execute("seq.some(tuple(false,true,false), identity)"));
    assertFalse((Boolean) this.instance.execute("seq.every(tuple(true,false,true), identity)"));
    assertTrue((Boolean) this.instance.execute("seq.not_any(tuple(false,false,false), identity)"));
    assertFalse((Boolean) this.instance.execute("seq.not_any(tuple(true,false,true), identity)"));

    // map and reduce with hash-map
    List<Object> results = (List<Object>) this.instance
        .execute("a=seq.map('k1', 'v1', 'k2', 'v2');" + "map(a,lambda(x) -> x.value end)");
    assertEquals(2, results.size());
    assertTrue(results.contains("v1"));
    assertTrue(results.contains("v2"));

    String result = (String) this.instance.execute("a=seq.map('k1', 'v1', 'k2', 'v2');"
        + "reduce(a,lambda(r, x) -> r+',' + x.key+ '=' + x.value end, '')");
    assertEquals(result, ",k1=v1,k2=v2");
  }

  @Test
  public void testIdentityFunction() {
    assertNull(this.instance.execute("identity(nil)"));
    assertEquals(1L, this.instance.execute("identity(1)"));
    assertEquals("hello", this.instance.execute("identity('hello')"));
  }

  @Test
  public void testIssue2() {
    assertEquals(100000000000000000000.0 / 3.0,
        this.instance.execute("100000000000000000000.0/3.0"));
    System.out.println(this.instance.execute("100000000000000000000.0/3.0"));
    // assertEquals(100000000000000000000/3,
    // instance.execute("100000000000000000000/3"));
  }


  @Test
  public void testStringFunction() {
    String s1 = "hello world";
    String s2 = "just for fun";
    String s3 = "aviator";

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("s1", s1);
    env.put("s2", s2);
    env.put("s3", s3);

    assertEquals("hello world aviator", this.instance.execute("'hello'+' '+'world'+' '+'aviator'"));
    assertEquals(4, this.instance.execute("string.length(\"fuck\")"));
    assertEquals(0, this.instance.execute("string.length('')"));
    assertEquals(19, this.instance.execute("string.length('hello'+' '+'world'+' '+'aviator')"));
    assertTrue((Boolean) this.instance.execute("string.contains('hello','he')"));
    assertFalse((Boolean) this.instance.execute("string.contains('hello','c')"));
    assertTrue((Boolean) this.instance.execute("string.startsWith('hello','he')"));
    assertFalse((Boolean) this.instance.execute("string.startsWith('hello','llo')"));
    assertFalse((Boolean) this.instance.execute("string.endsWith('hello','he')"));
    assertTrue((Boolean) this.instance.execute("string.endsWith('hello','llo')"));

    assertEquals("ello", this.instance.execute("string.substring('hello',1)"));
    assertEquals("el", this.instance.execute("string.substring('hello',1,3)"));

    // test with variable
    assertEquals("hello world aviator", this.instance.execute("s1+' '+s3", env));
    assertEquals(19, this.instance.execute("string.length(s1+' '+s3)", env));
    assertFalse((Boolean) this.instance.execute("string.startsWith(s1,'fuck')", env));
    assertTrue((Boolean) this.instance.execute("string.startsWith(s1,s1)", env));
    assertTrue((Boolean) this.instance.execute("string.endsWith(s1+s2,s2)", env));
    assertTrue((Boolean) this.instance.execute("string.contains(s1+s2,s1)", env));
    assertTrue((Boolean) this.instance.execute("string.contains(s1+s2,'world')", env));
    assertFalse((Boolean) this.instance.execute("string.contains(s1+s3,s2)", env));
    assertTrue((Boolean) this.instance.execute("string.contains(s1+s2+s3,s2)", env));
    assertEquals("ello world", this.instance.execute("string.substring(s1,1)", env));
    assertEquals("el", this.instance.execute("string.substring(s1,1,3)", env));
    assertEquals("hello", ((String[]) this.instance.exec("string.split('hello world',' ')"))[0]);
    assertEquals("world", ((String[]) this.instance.exec("string.split('hello world',' ')"))[1]);
  }


  @Test
  public void testBitOperations() {
    assertEquals(99 | 7, this.instance.execute("99|7"));
    assertEquals(99 | ~7, this.instance.execute("99|~7"));
    assertEquals(99 & 7, this.instance.execute("99&7"));
    assertEquals(99 ^ 7, this.instance.execute("99^7"));
    assertEquals(99 << 7, this.instance.execute("99<<7"));
    assertEquals(99 >> 7, this.instance.execute("99>>7"));
    assertEquals(99 >>> 7, this.instance.execute("99>>>7"));
    assertEquals(1 ^ 2 ^ 3 & 4 | 5 ^ ~2 | 5 & 4, this.instance.execute("1^2^3&4|5^~2|5&4"));
    assertEquals((1 ^ 2 ^ 3 & 4 | 5 ^ ~2 | 5 & 4) == 100,
        this.instance.execute("(1^2^3&4|5^~2|5&4) == 100"));
    assertEquals(
        4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? !false && true ? 1 & 4 : 0
            : 6L >> 2L * 2L / 4L ^ ~699L + 100L << 4L >> 5L >> 1000L,
        this.instance.execute(
            "4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? (!false && true ? 1 & 4 : 0) : 6 >> 2 * 2 / 4^ ~699 + 100 << 4 >> 5 >> 1000"));

    assertEquals((99 & 7) == (99 & 7) && false, this.instance.execute("(99&7)==(99&7)&&false "));
    assertEquals((99 | 7) != (99 | 7) || false, this.instance.execute("(99|7)!=(99|7)||false "));
  }


  @Test
  public void testBitOperationsWithVariable() {
    Map<String, Object> env = new HashMap<String, Object>();
    long i = 100;
    long j = -99;
    long k = 7;
    env.put("i", i);
    env.put("j", j);
    env.put("k", k);

    assertEquals(i | j, this.instance.execute("i|j", env));
    assertEquals(99 | k, this.instance.execute("99|k", env));
    assertEquals(i & j, this.instance.execute("i&j", env));
    assertEquals(99 & k, this.instance.execute("99&k", env));
    assertEquals(i ^ j, this.instance.execute("i^j", env));
    assertEquals(99 ^ k, this.instance.execute("99^k", env));
    assertEquals(i | ~j, this.instance.execute("i|~j", env));
    assertEquals(99 | ~k, this.instance.execute("99|~k", env));
    assertEquals(j >>> i, this.instance.execute("j>>>i", env));

    assertEquals(i ^ j ^ k & i & j & k | i | j | k & 3 & 4 | 5 & ~i,
        this.instance.execute("i^j^k&i&j&k|i|j|k&3&4|5&~i", env));
    assertEquals(
        4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? !false && true ? 1 & 4 : 0
            : i >> j * k / i ^ ~j + k << i >> j >> 1000L,
        this.instance.execute(
            "4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? (!false && true ? 1 & 4 : 0) :i >> j * k / i ^ ~j + k << i >> j >> 1000",
            env));

    assertEquals((i & 7) == (i & 7) && false,
        this.instance.execute("(i & 7) == (i & 7) && false ", env));
    assertEquals((j | k) != (j | k) || false,
        this.instance.execute("(j | k) != (j | k) || false ", env));
  }


  @Test
  public void testHexNumber() {
    Map<String, Object> env = new HashMap<String, Object>();
    long i = 100;
    float j = -99;
    int k = 7;
    env.put("i", i);
    env.put("j", j);
    env.put("k", k);

    assertEquals(0xA3, this.instance.execute("0xA3", env));
    assertEquals(0xA3 * 0x45 + 2, this.instance.execute("0xA3 * 0x45+2", env));
    assertEquals(0xFF == 0Xff, this.instance.execute("0xFF==0Xff", env));
    assertEquals(~0xFF == 0Xff, this.instance.execute("~0xFF==0Xff", env));
    assertEquals(~0xFF | k & 3 - 0X11, this.instance.execute("~0xFF|k&3-0X11", env));
    assertEquals(0x45 > i ? 0x11 - 0344 * 5 / 7 : k / 0xFF - j * 0x45,
        this.instance.execute("0x45>i?0x11-0344*5/7:k/0xFF-j*0x45 ", env));
  }


  @Test
  public void testBitOp_BitMap() {
    Map<String, Object> env = new HashMap<String, Object>();
    int flag = 0;
    env.put("flag", flag);
    assertEquals(false, this.instance.execute("(flag & 0x3E0) >> 5 ==15 ", env));
    flag = flag & 0xFFFFC1F | 15 << 5;
    env.put("flag", flag);
    assertEquals(true, this.instance.execute("(flag & 0x3E0) >> 5 ==15 ", env));

    assertEquals(false, this.instance.execute(" (flag & 0x400) >> 10 ==1 ", env));
    flag = flag & 0xFFFFFBFF | 1 << 10;
    env.put("flag", flag);
    assertEquals(true, this.instance.execute("(flag & 0x400) >> 10 ==1 ", env));
    assertEquals(true,
        this.instance.execute("(flag & 0x400) >> 10 ==1 && (flag & 0x3E0) >> 5 ==15", env));
    assertEquals(false,
        this.instance.execute("(flag & 0x400) >> 10 ==0 && (flag & 0x3E0) >> 5 ==15", env));

    assertEquals(0L, this.instance.execute(" ((flag & 0x1800) >> 11)", env));
    flag = flag & 0xFFFFE7FF | 1 << 11;
    env.put("flag", flag);
    assertEquals(1L, this.instance.execute(" ((flag & 0x1800) >> 11)", env));
    flag = flag & 0xFFFFE7FF | 2 << 11;
    env.put("flag", flag);
    assertEquals(2L, this.instance.execute(" ((flag & 0x1800) >> 11)", env));
    assertEquals(flag & 0xFFFFE7FF | 3 << 11,
        this.instance.execute(" flag & 0xFFFFE7FF | 3 << 11", env));
    flag = flag & 0xFFFFE7FF | 3 << 11;

    env.put("flag", flag);
    assertEquals(3L, this.instance.execute(" ((flag & 0x1800) >> 11)", env));
  }


  @Test
  public void testMathFunction() {
    assertEquals(Math.pow(10, 100.0), this.instance.exec("math.pow(10,100)"));
    assertEquals(Math.log(99), this.instance.exec("math.log(99)"));
    assertEquals(Math.log10(99), this.instance.exec("math.log10(99)"));
    assertEquals(Math.sin(99), this.instance.exec("math.sin(99)"));
    assertEquals(Math.cos(99), this.instance.exec("math.cos(99)"));
    assertEquals(Math.tan(99), this.instance.exec("math.tan(99)"));
    assertEquals(Math.sqrt(99), this.instance.exec("math.sqrt(99)"));
    assertEquals(Math.round(99.9), this.instance.exec("math.round(99.9)"));
    assertEquals(Math.round(99.1), this.instance.exec("math.round(99.1)"));
  }


  @Test
  public void testParseScientificNotations() {
    assertEquals(1e5, this.instance.exec("1e5"));
    assertEquals(1E5, this.instance.exec("1E5"));
    assertEquals(1E-5, this.instance.exec("1E-5"));

    assertEquals(2e3 + 4e6, this.instance.exec("2e3+4e6"));
    assertEquals(2e3 - 4e6, this.instance.exec("2e3-4e6"));
    assertEquals(2e3 / 4e6, this.instance.exec("2e3/4e6"));
    assertEquals(2e3 % 4e6, this.instance.exec("2e3%4e6"));
  }


  @Test
  public void testParseBigNumbers() {
    assertEquals(new BigInteger("99999999999999999999999999999999"),
        this.instance.exec("99999999999999999999999999999999"));
    assertEquals(new BigInteger("99999999999999999999999999999999"),
        this.instance.exec("99999999999999999999999999999999N"));
    assertEquals(new BigInteger("199999999999999999999999999999998"),
        this.instance.exec("99999999999999999999999999999999+99999999999999999999999999999999"));

    Env env = new Env(null);
    env.setInstance(this.instance);
    assertEquals(
        new BigDecimal("99999999999999999999999999999999.99999999",
            RuntimeUtils.getMathContext(env)),
        this.instance.exec("99999999999999999999999999999999.99999999M"));
  }

  @Test
  public void testGetVariableNamesConcurrently() throws Exception {
    final Expression exp = this.instance.compile("{let a = 1; let b = b + 1; p(a+b);} c-a");

    int threads = 30;

    final CyclicBarrier barrier = new CyclicBarrier(threads + 1);
    final AtomicInteger c = new AtomicInteger(0);
    for (int i = 0; i < threads; i++) {
      new Thread() {
        @Override
        public void run() {
          try {
            barrier.await();
            List<String> vars = exp.getVariableNames();
            assertEquals(3, vars.size());
            assertEquals("b", vars.get(0));
            assertEquals("c", vars.get(1));
            assertEquals("a", vars.get(2));
            c.incrementAndGet();
            barrier.await();
          } catch (Exception e) {
            e.printStackTrace();
            fail();
          }
        }
      }.start();
    }
    barrier.await();
    barrier.await();
    assertEquals(threads, c.get());
  }

  @Test
  public void testGetVariableNamesAndFunctionNamesComplex() {
    // lambda
    Expression exp = this.instance.compile("a = 1; add = lambda(x) -> x + a end; add(b)");
    List<String> vars = exp.getVariableNames();
    List<String> funcs = getFuncs(exp);
    assertEquals(1, vars.size());
    assertEquals("b", vars.get(0));
    assertEquals(0, funcs.size());

    // lambda closure over
    exp = this.instance.compile("add = lambda(x) -> x + a end; add(b)");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(2, vars.size());
    assertEquals("a", vars.get(0));
    assertEquals("b", vars.get(1));
    assertEquals(0, funcs.size());

    // if.. else
    exp = this.instance
        .compile("b=2; if(a > 1) { a + b } elsif( a > 10) { return a + c; } else { return 10; }");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(2, vars.size());
    assertEquals("a", vars.get(0));
    assertEquals("c", vars.get(1));
    assertEquals(0, funcs.size());

    // for..loop
    exp = this.instance
        .compile("let list = seq.list(1, 2, 3); for x in list { sum = sum + x }; return sum;");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(1, vars.size());
    assertEquals("sum", vars.get(0));
    assertEquals(1, funcs.size());
    assertEquals(Arrays.asList("seq.list"), funcs);

    // let statement in block
    exp = this.instance.compile("{let a = 1; let b = b + 1; p(a+b);} c-a");
    funcs = getFuncs(exp);
    vars = exp.getVariableNames();
    assertEquals(3, vars.size());
    assertEquals("b", vars.get(0));
    assertEquals("c", vars.get(1));
    assertEquals("a", vars.get(2));
    assertEquals(Arrays.asList("p"), funcs);

    // redfine variable
    exp = this.instance.compile("{let a = 1; let b = 2; let b = b + 1; p(a+b);} c-a");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(2, vars.size());
    assertEquals("c", vars.get(0));
    assertEquals("a", vars.get(1));
    assertEquals(Arrays.asList("p"), funcs);

    // high-order function
    exp = this.instance.compile("map(list, lambda(v) -> v + u end)");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(2, vars.size());
    assertEquals("list", vars.get(0));
    assertEquals("u", vars.get(1));
    assertEquals(Arrays.asList("map"), funcs);

    // a complex script
    exp = this.instance.compile("let n = 0;  let index =0 ; " + "for i in a {"
        + "let t = string_to_date(i, 'yyyyMMdd'); " + "if t == nil { continue; } "
        + "let m = date.month(b, i); " + "if c[index] == '03' && m <= 12 {" + " n = n + 1; " + "}"
        + "index = index + 1;" + "  } return n;");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(3, vars.size());
    assertEquals("a", vars.get(0));
    assertEquals("b", vars.get(1));
    assertEquals("c", vars.get(2));
    assertEquals(Arrays.asList("date.month", "string_to_date"), funcs);

    exp = this.instance.compile("a = seq.map(); add = lambda() -> a.b + a.c end; add()");
    vars = exp.getVariableNames();
    funcs = getFuncs(exp);
    assertEquals(0, vars.size());
    vars = exp.getVariableFullNames();
    assertEquals(0, vars.size());
    assertEquals(Arrays.asList("seq.map"), funcs);

    exp = this.instance.compile("a = seq.map(); a.c = 2; add = lambda() -> a.b + a.c end; add()");
    funcs = getFuncs(exp);
    vars = exp.getVariableNames();
    assertEquals(0, vars.size());
    vars = exp.getVariableFullNames();
    assertEquals(0, vars.size());
    assertEquals(Arrays.asList("seq.map"), funcs);

    exp = this.instance.compile("add = lambda(a) -> a.b + test1(a.c) + test2(y.d) end; add(x)");
    funcs = getFuncs(exp);
    vars = exp.getVariableNames();
    assertEquals(2, vars.size());
    assertEquals("y", vars.get(0));
    assertEquals("x", vars.get(1));
    vars = exp.getVariableFullNames();
    assertEquals(2, vars.size());
    assertEquals("y.d", vars.get(0));
    assertEquals("x", vars.get(1));
    assertEquals(2, funcs.size());
    assertEquals(Arrays.asList("test1", "test2"), funcs);
  }

  private List<String> getFuncs(Expression exp) {
    List<String> funcs = exp.getFunctionNames();
    Collections.sort(funcs);
    return funcs;
  }

  @Test
  public void testIssue431() {
    Expression expr = this.instance.compile("{\n" + "    let itemv1 = seq.get(info.value,index);\n"
        + "    print( itemv1.name )\n" + "}");

    List<String> vars = expr.getVariableNames();
    assertEquals(2, vars.size());
    assertEquals("info", vars.get(0));
    assertEquals("index", vars.get(1));
  }


  @Test
  public void testGetVariableNames() {
    Expression expression = this.instance.compile("b+a", true);
    assertNotNull(expression);
    List<String> vars = expression.getVariableNames();
    assertNotNull(vars);
    assertEquals(2, vars.size());
    assertTrue(vars.contains("a"));
    assertTrue(vars.contains("b"));
    assertEquals("b", vars.get(0));
    assertEquals("a", vars.get(1));

    expression = this.instance.compile("b==a || d>3 || e+c*d/2 <= 1000", true);
    assertNotNull(expression);
    vars = expression.getVariableNames();
    assertNotNull(vars);
    assertEquals(5, vars.size());
    assertTrue(vars.contains("a"));
    assertTrue(vars.contains("b"));
    assertTrue(vars.contains("c"));
    assertTrue(vars.contains("d"));
    assertTrue(vars.contains("e"));
    assertEquals("b", vars.get(0));
    assertEquals("a", vars.get(1));
    assertEquals("d", vars.get(2));
    assertEquals("e", vars.get(3));
    assertEquals("c", vars.get(4));

    // Test map or list as variable
    expression =
        this.instance.compile("map.a>10 && list[0][1]<3 && bean.c == bean.x || bean.d == y", true);
    assertNotNull(expression);
    vars = expression.getVariableNames();
    assertEquals(4, vars.size());
    assertTrue(vars.contains("map"));
    assertTrue(vars.contains("list"));
    assertTrue(vars.contains("bean"));
    assertTrue(vars.contains("y"));
    assertEquals("map", vars.get(0));
    assertEquals("list", vars.get(1));
    assertEquals("bean", vars.get(2));
    assertEquals("y", vars.get(3));

  }


  @Test
  public void testArrayAccess() {

    // instance.setTrace(true);
    Map<String, Object> env = new HashMap<String, Object>();
    int[] a = new int[] {1, 2, 3, 4};
    int[][] b = new int[][] {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    int[][][] c = new int[][][] {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}};
    env.put("a", a);
    env.put("b", b);
    env.put("c", c);

    assertEquals(a[0] + b[0][0] + c[0][0][0],
        this.instance.execute("a[0]+b[0][0]+c[0][0][0]", env));
    assertEquals(a[1] + b[0][2] * c[1][1][1],
        this.instance.execute("a[1]+b[0][2]*c[1][1][1]", env));
    assertEquals(a[1] + b[0][2] * c[1][1][1] / (a[2] * a[1] + 100 - c[0][1][1] * b[0][1]),
        this.instance.execute("a[1]+b[0][2]*c[1][1][1]/(a[2]*a[1]+100-c[0][1][1]*b[0][1])", env));
    assertEquals(c[0][1][1] > b[1][0], this.instance.execute("c[0][1][1]>b[1][0]", env));
    assertEquals(c[0][1][1] <= b[1][0], this.instance.execute("c[0][1] [1] <= b[1][0]", env));
    assertEquals(c[0][1][1] > b[1][0] ? a[0] : a[2],
        this.instance.execute("c[0][1][1]>b[1][0]? a[0]:a[2]", env));
    assertEquals(b[0].length, this.instance.execute("count(b[0])", env));
    assertEquals(6, this.instance.execute("reduce(b[0],+,0)", env));
    Object[] rt = (Object[]) this.instance.execute("filter(c[0][0],seq.gt(1))", env);
    assertEquals(1, rt.length);
    assertEquals(2, rt[0]);
    this.instance.execute("map(c[1][0],println)", env);
    assertTrue((Boolean) this.instance.execute("include(b[0],3)", env));

  }


  @Test
  public void testBigNumber() {
    // big int + long
    assertEquals(new BigInteger("4"), this.instance.exec("a+b", 1, new BigInteger("3")));
    assertEquals(new BigInteger("4"), this.instance.exec("a+3N", 1));
    assertEquals(new BigInteger("4"), this.instance.exec("1+b", new BigInteger("3")));
    assertEquals(new BigInteger("4"), this.instance.exec("3N+1"));
    assertEquals(new BigInteger("300"), this.instance.exec("3N*100"));
    assertEquals(new BigInteger("100"), this.instance.exec("400/4N"));
    assertEquals(new BigInteger("-3"), this.instance.exec("a-4N", 1));

    // big int + double
    assertEquals(4.1, this.instance.exec("a+b", 1.1, new BigInteger("3")));
    assertEquals(4.1, this.instance.exec("a+3N", 1.1));
    assertEquals(4.1, this.instance.exec("1.1+b", new BigInteger("3")));
    assertEquals(4.1, this.instance.exec("3N+1.1"));
    assertEquals(300.0, this.instance.exec("3N*100.0"));
    assertEquals(100.0, this.instance.exec("400.0/4N"));
    assertEquals(-2.9, this.instance.exec("a-4N", 1.1));

    // big int + big int
    assertEquals(new BigInteger("4"),
        this.instance.exec("a+b", new BigInteger("1"), new BigInteger("3")));
    assertEquals(new BigInteger("4"), this.instance.exec("a+3N", new BigInteger("1")));
    assertEquals(new BigInteger("4"), this.instance.exec("1+b", new BigInteger("3")));
    assertEquals(new BigInteger("4"), this.instance.exec("3N+1N"));
    assertEquals(new BigInteger("300"), this.instance.exec("3N*100N"));
    assertEquals(new BigInteger("100"), this.instance.exec("400N/4N"));
    assertEquals(new BigInteger("-3"), this.instance.exec("a-4N", new BigInteger("1")));

    // big int + decimal
    assertEquals(new BigDecimal("4.1"),
        this.instance.exec("a+b", new BigDecimal("1.1"), new BigInteger("3")));
    assertEquals(new BigDecimal("4"), this.instance.exec("a+3N", new BigDecimal("1")));
    assertEquals(new BigDecimal("4.1"), this.instance.exec("1.1M+b", new BigInteger("3")));
    assertEquals(new BigDecimal("4.1"), this.instance.exec("3N+1.1M"));
    assertEquals(new BigDecimal("301.00"), this.instance.exec("3.01M*100N"));
    assertEquals(new BigDecimal("100"), this.instance.exec("400M/4N"));
    assertEquals(new BigDecimal("-2.9"), this.instance.exec("a-4N", new BigDecimal("1.1")));

    // decimal + long
    assertEquals(new BigDecimal("4.1"), this.instance.exec("a+b", new BigDecimal("1.1"), 3));
    assertEquals(new BigDecimal("4"), this.instance.exec("a+3", new BigDecimal("1")));
    assertEquals(new BigDecimal("4.1"), this.instance.exec("1.1M+b", 3));
    assertEquals(new BigDecimal("4.1"), this.instance.exec("3+1.1M"));
    assertEquals(new BigDecimal("301.00"), this.instance.exec("3.01M*100"));
    assertEquals(new BigDecimal("100"), this.instance.exec("400M/4"));
    assertEquals(new BigDecimal("-2.9"), this.instance.exec("a-4", new BigDecimal("1.1")));
    // decimal + double
    assertEquals(4.1, this.instance.exec("a+b", 1.1, new BigDecimal("3")));
    assertEquals(4.1, this.instance.exec("a+3.0M", 1.1));
    assertEquals(4.1, this.instance.exec("1.1+b", new BigDecimal("3")));
    assertEquals(4.1, this.instance.exec("3.00M+1.1"));
    assertEquals(300.0, this.instance.exec("3M*100.0"));
    assertEquals(100.0, this.instance.exec("400.0/4M"));
    assertEquals(-2.9, this.instance.exec("a-4.00M", 1.1));
  }


  @Test
  public void testBigNumberNegative() {
    assertEquals(new BigInteger("-1000000000000000000000000000000000"),
        this.instance.exec("-a", new BigInteger("1000000000000000000000000000000000")));
    assertEquals(new BigDecimal("9999999999999999999999999999999999999.99999999999"), this.instance
        .exec("-a", new BigDecimal("-9999999999999999999999999999999999999.99999999999")));
    assertEquals(new BigDecimal("9999999999999999999.999999999999"),
        this.instance.exec("-(-9999999999999999999.999999999999M)"));
    assertEquals(new BigInteger("9999999999999999999"),
        this.instance.exec("-(-9999999999999999999N)"));
  }


  @Test
  public void testBigNumberBitOperations() {
    assertEquals(
        new BigInteger("1000000000000000000000000000000000")
            .xor(new BigInteger("9999999999999999999999")),
        this.instance.exec("a^b", new BigInteger("1000000000000000000000000000000000"),
            new BigInteger("9999999999999999999999")));
    assertEquals(
        new BigInteger("1000000000000000000000000000000000")
            .and(new BigInteger("9999999999999999999999")),
        this.instance.exec("a&b", new BigInteger("1000000000000000000000000000000000"),
            new BigInteger("9999999999999999999999")));
    assertEquals(
        new BigInteger("1000000000000000000000000000000000")
            .or(new BigInteger("9999999999999999999999")),
        this.instance.exec("a|b", new BigInteger("1000000000000000000000000000000000"),
            new BigInteger("9999999999999999999999")));
    assertEquals(new BigInteger("1000000000000000000000000000000000").shiftLeft(2),
        this.instance.exec("a<<2", new BigInteger("1000000000000000000000000000000000")));
    assertEquals(new BigInteger("1000000000000000000000000000000000").shiftRight(2),
        this.instance.exec("a>>2", new BigInteger("1000000000000000000000000000000000")));
    assertEquals(new BigInteger("1000000000000000000000000000000000").shiftRight(2),
        this.instance.exec("a>>>2", new BigInteger("1000000000000000000000000000000000")));
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testDecimalBitAnd() {
    this.instance.exec("3M< & 2M");
  }


  @Test
  public void testAlwaysUseDoubleAsDecimal() {
    this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    try {
      Object val = this.instance.execute("3.2");
      assertTrue(val instanceof BigDecimal);
      assertEquals(new BigDecimal("3.2"), val);

      val = this.instance.execute("3.2 + 4.3");
      assertTrue(val instanceof BigDecimal);
      assertEquals(new BigDecimal("7.5"), val);

      Map<String, Object> env = new HashMap<String, Object>();
      env.put("a", new BigDecimal("2.1"));
      env.put("b", 4);
      val = this.instance.execute("3.2 + a * b ", env);
      assertTrue(val instanceof BigDecimal);
      assertEquals(new BigDecimal("11.6"), val);
    } finally {
      this.instance.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
    }
  }


  @Test
  public void testOtherFunction() {
    // instance.setOptimize(instance.EVAL);
    // System.setProperty("aviator.asm.trace","true");
    assertTrue(
        (Boolean) this.instance.execute("'A' == 'A' || 'B' == 'B' && 'ABCD' == t &&  'A' == 'A'"));

  }

  @Test
  public void testDisablePropertySyntaxSugar() {
    Map<String, Object> env = createUsersEnv();
    String username = (String) this.instance.execute("#data.[0].name", env);
    assertEquals(username, "张三");
    this.instance.setOption(Options.ENABLE_PROPERTY_SYNTAX_SUGAR, false);
    assertNull(this.instance.execute("#data.[0].name", env));
    this.instance.setOption(Options.ENABLE_PROPERTY_SYNTAX_SUGAR, true);
  }

  @Test
  public void testPropertyNilNotFound() {
    Map<String, Object> env = createUsersEnv();
    try {
      this.instance.execute("#data[3].age", env);
      fail();
    } catch (ExpressionRuntimeException e) {
      assertTrue(true);
    }
    this.instance.setOption(Options.NIL_WHEN_PROPERTY_NOT_FOUND, true);
    assertNull(this.instance.execute("#data[3].age", env));
  }

  @Test
  public void testSeqFilterListWithProperty() {
    Map<String, Object> env = createUsersEnv();
    Object result =
        this.instance.execute("filter(data,seq.and(seq.gt(25,'age'),seq.eq('李四','name')))", env);
    List list = (List) result;
    assertEquals(1, list.size());
    for (Object o : list) {
      User user = (User) o;
      assertEquals("李四", user.getName());
      assertTrue(user.getAge() > 25);
    }
  }


  private Map<String, Object> createUsersEnv() {
    List<User> users = new ArrayList<>(3);
    users.add(new User(1L, 25, "张三"));
    users.add(new User(2L, 26, "李四"));
    users.add(new User(3L, 27, "王五"));
    Map<String, Object> env = new HashMap<>();
    env.put("data", users);
    return env;
  }

  @Test
  public void testSeqFilterMapWithProperty() {
    Map<Long, User> idUserMap = new HashMap<>(3);
    idUserMap.put(1L, new User(1L, 25, "张三"));
    idUserMap.put(2L, new User(2L, 26, "李四"));
    idUserMap.put(3L, new User(3L, 27, "王五"));
    Map<String, Object> env = new HashMap<>();
    env.put("data", idUserMap);
    Object result = this.instance
        .execute("filter(data,seq.and(seq.gt(25,'value.age'),seq.eq('李四','value.name')))", env);
    Map map = (Map) result;
    assertEquals(1, map.size());
    for (Object o : map.values()) {
      User user = (User) o;
      assertEquals("李四", user.getName());
      assertTrue(user.getAge() > 25);
    }
  }

  @Test
  public void testTuple() {
    assertArrayEquals(new Object[] {1, "hello", 3.2},
        (Object[]) this.instance.execute("tuple(1,'hello',3.2)"));
    assertArrayEquals(new Object[] {1, 2}, (Object[]) this.instance.execute("tuple(1,2)"));
    assertArrayEquals(new Object[] {}, (Object[]) this.instance.execute("tuple()"));
    assertEquals(3, this.instance.execute("count(tuple(1,'hello',3.2))"));
    assertEquals(3.2, this.instance.execute("tuple(1,'hello',3.2)[2]"));
    assertArrayEquals(new Object[] {2, 3, 4},
        (Object[]) this.instance.execute("map(tuple(1,2,3), lambda(x) -> x +1 end)"));

    assertEquals(1, this.instance.execute("seq.get(tuple(1,'hello',3.2), 0)"));
    assertEquals("hello", this.instance.execute("seq.get(tuple(1,'hello',3.2), 1)"));
    assertEquals(3.2, this.instance.execute("seq.get(tuple(1,'hello',3.2), 2)"));

    try {
      assertEquals(1, this.instance.execute("seq.get(tuple(1,'hello',3.2), 3)"));
      fail();
    } catch (ArrayIndexOutOfBoundsException e) {

    }
  }


  @Test
  public void testSeqMinMaxFunction() {
    assertEquals(-1, this.instance.execute("seq.min(tuple(4,2,3,-1,5))"));
    assertEquals(5, this.instance.execute("seq.max(tuple(4,2,3,1,5))"));


    assertEquals(null, this.instance.execute("seq.min(tuple())"));
    assertEquals(null, this.instance.execute("seq.max(tuple())"));

    assertEquals(99, this.instance.execute("seq.min(tuple(99))"));
    assertEquals(99, this.instance.execute("seq.max(tuple(99))"));

    assertEquals(null, this.instance.execute("seq.min(tuple(nil))"));
    assertEquals(null, this.instance.execute("seq.max(tuple(nil))"));

    assertEquals(null, this.instance.execute("seq.min(tuple(4,nil,3,-1,5))"));
    assertEquals(5, this.instance.execute("seq.max(tuple(4,2,nil,-1,5))"));

    try {
      assertEquals(5, this.instance.execute("seq.max(tuple(4,'hello',3,1,5))"));
      fail();
    } catch (CompareNotSupportedException e) {
      assertEquals("Could not compare `hello` with `4`", e.getMessage());
    }

    try {
      assertEquals(5, this.instance.execute("seq.min(tuple(4,'hello',3,1,5))"));
      fail();
    } catch (CompareNotSupportedException e) {
      assertEquals("Could not compare `hello` with `4`", e.getMessage());
    }

    Map<String, Object> env = new HashMap<>();
    env.put("a", Arrays.asList(4, 3, 5, -6, 9));
    assertEquals(-6, this.instance.execute("seq.min(a)", env));
    assertEquals(9, this.instance.execute("seq.max(a)", env));

    env.put("a", Arrays.asList(4, 3, 5, null, -6, 9));
    assertEquals(null, this.instance.execute("seq.min(a)", env));
    assertEquals(9, this.instance.execute("seq.max(a)", env));

    try {
      env.put("a", Arrays.asList(4, 3, 5, "hello", -6, 9));
      assertEquals(5, this.instance.execute("seq.min(a)", env));
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals("Could not compare `hello` with `3`", e.getMessage());
    }

    try {
      env.put("a", Arrays.asList(4, 3, 5, "hello", -6, 9));
      assertEquals(5, this.instance.execute("seq.max(a)", env));
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals("Could not compare `hello` with `5`", e.getMessage());
    }
    env.put("a", null);
    assertEquals(null, this.instance.execute("seq.min(a)", env));
    assertEquals(null, this.instance.execute("seq.max(a)", env));

    env.put("a", Arrays.asList());
    assertEquals(null, this.instance.execute("seq.min(a)", env));
    assertEquals(null, this.instance.execute("seq.max(a)", env));

    env.put("a", Arrays.asList(4));
    assertEquals(4, this.instance.execute("seq.min(a)", env));
    assertEquals(4, this.instance.execute("seq.max(a)", env));

    try {
      env.put("a", 3);
      assertEquals(5, this.instance.execute("seq.max(a)", env));
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("3 is not a sequence", e.getMessage());
    }

    try {
      env.put("a", 3);
      assertEquals(5, this.instance.execute("seq.min(a)", env));
      fail();
    } catch (IllegalArgumentException e) {
      assertEquals("3 is not a sequence", e.getMessage());
    }
  }


  private List newList(final Object... args) {
    List list = new ArrayList<>();
    for (Object obj : args) {
      list.add(obj);
    }
    return list;
  }

  private Set newSet(final Object... args) {
    Set list = new HashSet<>();
    for (Object obj : args) {
      list.add(obj);
    }
    return list;
  }

  @Test
  public void testSeqNewArray() {
    assertArrayEquals(new String[] {},
        (String[]) this.instance.execute("seq.array(java.lang.String)"));

    Assert.assertArrayEquals(new long[] {1, 2, 3},
        (long[]) this.instance.execute("seq.array(long, 1,2,3)"));

    Assert.assertArrayEquals(new short[] {-2, 3, 100},
        (short[]) this.instance.execute("seq.array(short, -2, 3, 100)"));

    assertEquals(101L, (long) this.instance.execute("reduce(seq.array(short, -2, 3, 100), +, 0)"));
  }

  @Test
  public void testSeqNewList() {
    assertEquals(newList(), this.instance.execute("seq.list()"));
    assertEquals(newList(1L), this.instance.execute("seq.list(1)"));
    assertEquals(newList(1L, 1L, 2L, 3L), this.instance.execute("seq.list(1,1,2,3)"));
    assertEquals(newList(1L, 2L, 3L, 4L), this.instance.execute("seq.list(1,2,3,4)"));
    assertEquals(newList(1L, 2.2, "hello"), this.instance.execute("seq.list(1,2.2, 'hello')"));

    assertEquals(newList(1L), this.instance.execute("seq.add(seq.list(), 1)"));
    assertEquals(newList(1L, "hello"),
        this.instance.execute("seq.add(seq.add(seq.list(), 1), 'hello')"));

    assertEquals(newList("hello"),
        this.instance.execute("seq.remove(seq.add(seq.add(seq.list(), 1), 'hello'), 1)"));
    assertEquals(newList(), this.instance.execute("seq.remove(seq.list(), nil)"));

    assertEquals(1, this.instance.execute("seq.get(seq.list(1,1,2,3),0)"));
    assertEquals(1, this.instance.execute("seq.get(seq.list(1,1,2,3),1)"));
    assertEquals(2, this.instance.execute("seq.get(seq.list(1,1,2,3),2)"));
    assertEquals(3, this.instance.execute("seq.get(seq.list(1,1,2,3),3)"));

    try {
      assertEquals(1, this.instance.execute("seq.get(seq.list(1,1,2,3),4)"));
      fail();
    } catch (IndexOutOfBoundsException e) {
      // Jdk changed the message of IndexOutOfBoundsException:
      // - for jdk 8-, cause exception is
      // java.lang.IndexOutOfBoundsException: Index: 4, Size: 4
      // - for jdk 9/10, exception is
      // java.lang.IndexOutOfBoundsException: Index 4 out-of-bounds for length 4
      // - for jdk 11, exception is
      // java.lang.IndexOutOfBoundsException: Index 4 out of bounds for length 4
      assertTrue(e.getMessage().equals("Index: 4, Size: 4")
          || e.getMessage().equals("Index 4 out-of-bounds for length 4")
          || e.getMessage().equals("Index 4 out of bounds for length 4"));
    }
  }

  @Test
  public void testSeqContainsKey() {
    assertEquals(Boolean.TRUE, this.instance.execute("seq.contains_key(seq.map(1,2,3,4), 1)"));
    assertEquals(Boolean.FALSE, this.instance.execute("seq.contains_key(seq.map(1,2,3,4), 2)"));
    assertEquals(Boolean.TRUE, this.instance.execute("seq.contains_key(seq.map(1,2,3,4), 3)"));
    assertEquals(Boolean.FALSE, this.instance.execute("seq.contains_key(seq.map(1,2,3,4), 10)"));

    Map<Object, Object> map = new HashMap<>();
    map.put("hello", 2L);
    map.put(3L, 4L);
    map.put("world", null);

    Map<String, Object> env = new HashMap<>();
    env.put("m", map);

    assertEquals(Boolean.TRUE, this.instance.execute("seq.contains_key(m, 'hello')", env));
    assertEquals(Boolean.TRUE, this.instance.execute("seq.contains_key(m, 'world')", env));
    assertEquals(Boolean.TRUE, this.instance.execute("seq.contains_key(m, 3)", env));
    assertEquals(Boolean.FALSE, this.instance.execute("seq.contains_key(m, 'test')", env));
    assertEquals(Boolean.FALSE, this.instance.execute("seq.contains_key(m, -1)", env));
  }

  @Test
  public void testSeqContainsKeyListArray() {
    assertEquals(false, this.instance.execute("seq.contains_key(seq.list(), 0)"));
    assertEquals(false, this.instance.execute("seq.contains_key(seq.list(), 2)"));
    assertEquals(true, this.instance.execute("seq.contains_key(seq.list(1,2), 0)"));
    assertEquals(true, this.instance.execute("seq.contains_key(seq.list(1,2), 1)"));
    assertEquals(false, this.instance.execute("seq.contains_key(seq.list(1,2), 2)"));
    assertEquals(false, this.instance.execute("seq.contains_key(seq.list(1,2), -1)"));

    assertEquals(false, this.instance.execute("seq.contains_key(tuple(), 0)"));
    assertEquals(false, this.instance.execute("seq.contains_key(tuple(), 2)"));
    assertEquals(true, this.instance.execute("seq.contains_key(tuple(1,2), 0)"));
    assertEquals(true, this.instance.execute("seq.contains_key(tuple(1,2), 1)"));
    assertEquals(false, this.instance.execute("seq.contains_key(tuple(1,2), 2)"));
    assertEquals(false, this.instance.execute("seq.contains_key(tuple(1,2), -1)"));
  }

  @Test
  public void testSeqNewMap() {
    Map<Object, Object> map = new HashMap<>();

    assertEquals(map, this.instance.execute("seq.map()"));

    map.put(1L, 2L);
    map.put(3L, 4L);
    assertEquals(map, this.instance.execute("seq.map(1,2,3,4)"));

    map.put("a", "b");
    assertEquals(map, this.instance.execute("seq.map(1,2,3,4,'a','b')"));

    map.clear();
    map.put(1L, 2L);
    map.put(3L, 4L);
    assertEquals(map, this.instance.execute("seq.add(seq.map(1,2),3,4)"));
    map.put("a", "b");
    assertEquals(map, this.instance.execute("seq.add(seq.map(1,2,3,4), 'a','b')"));

    map.remove(3L);
    assertEquals(map, this.instance.execute("seq.remove(seq.add(seq.map(1,2,3,4), 'a','b'), 3)"));

    map.remove("a");
    assertEquals(map, this.instance
        .execute("seq.remove(seq.remove(seq.add(seq.map(1,2,3,4), 'a','b'), 3), 'a')"));

    assertEquals(2, this.instance.execute("seq.get(seq.map(1,2,3,4,'a','b'), 1)"));
    assertEquals(null, this.instance.execute("seq.get(seq.map(1,2,3,4,'a','b'), 2)"));
    assertEquals(4, this.instance.execute("seq.get(seq.map(1,2,3,4,'a','b'), 3)"));
    assertEquals(null, this.instance.execute("seq.get(seq.map(1,2,3,4,'a','b'), 4)"));
    assertEquals("b", this.instance.execute("seq.get(seq.map(1,2,3,4,'a','b'), 'a')"));
  }

  @Test
  public void testIssue134() {
    Map<String, Object> env = new HashMap<>(2);
    env.put("v", 3);
    assertEquals(3, this.instance
        .execute("func=lambda(v)->v+2 end;func2=lambda(v)->func(v) end;func(1) ; func2(1)", env));
    assertEquals(6, this.instance.execute(
        "func=lambda(v)->v+2 end;func2=lambda(v)->func(v) end; func3 = lambda(v) -> func(v) + func2(v) end; func(1); func2(1);func3(1)",
        env));
  }

  @Test
  public void testSeqNewSet() {
    assertEquals(newSet(), this.instance.execute("seq.set()"));
    assertEquals(newSet(1L), this.instance.execute("seq.set(1)"));
    assertEquals(newSet(1L, 2L, 3L, 4L), this.instance.execute("seq.set(1,2,3,4)"));
    assertEquals(newSet(1L, 2.2, "hello"), this.instance.execute("seq.set(1,2.2, 'hello')"));

    assertEquals(newSet(1L), this.instance.execute("seq.add(seq.set(), 1)"));
    assertEquals(newSet(1L, "hello"),
        this.instance.execute("seq.add(seq.add(seq.set(), 1), 'hello')"));

    assertEquals(newSet("hello"),
        this.instance.execute("seq.remove(seq.add(seq.add(seq.set(), 1), 'hello'), 1)"));
    assertEquals(newSet(), this.instance.execute("seq.remove(seq.set(), nil)"));
    assertEquals(newSet(1L, 2L, 3L), this.instance.execute("seq.set(1,1,2,3)"));

    assertEquals(3, this.instance.execute("count(seq.set(1,1,2,3))"));
    assertEquals(1, this.instance.execute("seq.get(seq.set(1, 99 ,3), 1)"));
    assertEquals(1, this.instance.execute("seq.get(seq.set(1, 99 ,3), 1)"));
    assertEquals(99, this.instance.execute("seq.get(seq.set(1, 99 ,3),99)"));
    assertEquals(null, this.instance.execute("seq.get(seq.set(1, 99 ,3), 100)"));
    assertEquals(null, this.instance.execute("seq.get(seq.set(1, 99 ,3), 'hello')"));
  }

  @Test
  public void testSystemMinMaxFunction() {
    assertEquals(-1, this.instance.execute("min(4,2,3,-1,5)"));
    assertEquals(5, this.instance.execute("max(4,2,3,1,5)"));

    assertEquals(null, this.instance.execute("min()"));
    assertEquals(null, this.instance.execute("max()"));

    assertEquals(99, this.instance.execute("min(99)"));
    assertEquals(99, this.instance.execute("max(99)"));

    assertEquals(null, this.instance.execute("min(nil)"));
    assertEquals(null, this.instance.execute("max(nil)"));

    assertEquals(null, this.instance.execute("min(4,nil,3,-1,5)"));
    assertEquals(5, this.instance.execute("max(4,2,nil,-1,5)"));

    try {
      assertEquals(5, this.instance.execute("max(4,'hello',3,1,5)"));
      fail();
    } catch (CompareNotSupportedException e) {
      assertEquals("Could not compare <String, hello> with <Long, 4>", e.getMessage());
    }


    try {
      assertEquals(5, this.instance.execute("min(4,'hello',3,1,5)"));
      fail();
    } catch (CompareNotSupportedException e) {
      assertEquals("Could not compare <String, hello> with <Long, 4>", e.getMessage());
    }

    Map<String, Object> env = new HashMap<>();
    env.put("a", 1);
    env.put("b", -99.3);
    env.put("c", "hello");
    env.put("d", false);

    assertEquals(-99.3, this.instance.execute("min(4,a,3,b,1,5)", env));
    assertEquals(5, this.instance.execute("max(4,a,3,b,1,5)", env));

    assertEquals(null, this.instance.execute("min(4,nil, a,3,b,1,5)", env));
    assertEquals(5, this.instance.execute("max(4,nil, a,3,b,1,5)", env));

    assertEquals(1, this.instance.execute("min(a)", env));
    assertEquals(1, this.instance.execute("max(a)", env));

    try {
      assertEquals(5, this.instance.execute("max(a,b,c,5)", env));
      fail();
    } catch (ExpressionRuntimeException e) {
      assertEquals("Could not compare <String, hello> with <JavaType, a, 1, java.lang.Integer>",
          e.getMessage());
    }
  }

  @Test
  public void testOverloadLogicOperator() {
    // instance.setOption(Options.TRACE_EVAL, true);
    this.instance.addOpFunction(OperatorType.AND, new AbstractFunction() {

      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        return arg1.add(arg2, env);
      }

      @Override
      public String getName() {
        return "&&";
      }
    });
    this.instance.addOpFunction(OperatorType.OR, new AbstractFunction() {

      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        return arg1.sub(arg2, env);
      }

      @Override
      public String getName() {
        return "||";
      }
    });
    assertEquals(3, this.instance.execute("1 && 2"));
    assertEquals(6, this.instance.execute("1 && 2 && 3"));
    assertEquals(0, this.instance.execute("1 && 2 || 3"));
    assertEquals(-4, this.instance.execute("1 || 2 || 3"));
    this.instance.removeOpFunction(OperatorType.AND);
    this.instance.removeOpFunction(OperatorType.OR);
  }

  @Test
  public void testTypeFunctions() {
    assertEquals("long", this.instance.execute("type(1)"));
    assertEquals("double", this.instance.execute("type(1.1)"));
    assertEquals("decimal", this.instance.execute("type(1.1M)"));
    assertEquals("bigint", this.instance.execute("type(1N)"));
    assertEquals("pattern", this.instance.execute("type(/\\d+/)"));
    assertEquals("string", this.instance.execute("type('world')"));
    assertEquals("java.util.ArrayList", this.instance.execute("type(seq.list())"));
    assertEquals("java.lang.Object[]", this.instance.execute("type(tuple(1,2))"));
  }

  @Test
  public void testAssert() {
    this.instance.execute("assert(true)");
    try {
      this.instance.execute("assert(false)");
      fail();
    } catch (AssertFailed e) {

    }
    try {
      this.instance.execute("assert(false, 'test')");
      fail();
    } catch (AssertFailed e) {
      assertEquals("test", e.getMessage());
    }
  }



  public static class User {
    private Long id;
    private Integer age;
    private String name;

    public User(final Long id, final Integer age, final String name) {
      this.id = id;
      this.age = age;
      this.name = name;
    }

    public Long getId() {
      return this.id;
    }

    public void setId(final Long id) {
      this.id = id;
    }

    public Integer getAge() {
      return this.age;
    }

    public void setAge(final Integer age) {
      this.age = age;
    }

    public String getName() {
      return this.name;
    }

    public void setName(final String name) {
      this.name = name;
    }

    @Override
    public String toString() {
      return "User{" + "id=" + this.id + ", age=" + this.age + ", name='" + this.name + '\'' + '}';
    }
  }
}
