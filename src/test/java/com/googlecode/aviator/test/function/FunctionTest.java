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

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import org.junit.Test;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.*;
import static org.junit.Assert.*;


public class FunctionTest {
  @Test
  public void testArithmeticExpression() {
    assertEquals(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10,
        AviatorEvaluator.execute("1+2+3+4+5+6+7+8+9+10"));
    assertEquals(0, AviatorEvaluator.execute("1+2-3"));
    assertEquals(120, AviatorEvaluator.execute("1*2*3*4*5"));
    assertEquals(-4, AviatorEvaluator.execute("1-2-3"));
    assertEquals(2, AviatorEvaluator.execute("1-(2-3)"));

    assertEquals(50, AviatorEvaluator.execute("100/2"));
    assertEquals(33, AviatorEvaluator.execute("100/3"));

    assertEquals(-49, AviatorEvaluator.execute("1-100/2"));
    assertEquals(51, AviatorEvaluator.execute("1+100/2"));
    assertEquals(
        6 - (4 / 2 - (4 + 5)) * 2 + 100 / (2 + 1) * 20 - 5 * 5 * 5 + (6 + 1) / (2 - 3 / (1 + 1)),
        AviatorEvaluator.execute("6-(4/2-(4+5))*2+100/(2+1)*20-5*5*5+(6+1)/(2-3/(1+1))"));

    assertEquals(62.8, AviatorEvaluator.execute("2*3.14*10"));
    assertEquals(96.3, AviatorEvaluator.execute("100.3-4"));
    assertEquals(-96.3, AviatorEvaluator.execute("4-100.3"));
    assertEquals(100.3 / 4 - (4.0 / 2 + 5), AviatorEvaluator.execute("100.3/4-(4.0/2+5)"));

    assertEquals(1, AviatorEvaluator.execute("100%3"));
    assertEquals(0, AviatorEvaluator.execute("1-100%3"));
    assertEquals(100 % 3 * 4.2 + (37 + 95) / (6 * 3 - 18.0),
        (Double) AviatorEvaluator.execute("100%3*4.2+(37+95)/(6*3-18.0)"), 0.0001);
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
    System.out.println(AviatorEvaluator.execute("i+pi", env).getClass());

    System.setProperty("aviator.asm.trace", "true");
    assertEquals(-100, AviatorEvaluator.execute("-i", env));
    assertEquals(-103.4, AviatorEvaluator.execute("-i-pi", env));
    assertEquals(2 * 3.14 * 10, (Double) AviatorEvaluator.execute("2*pi*10", env), 0.001);
    assertEquals(3.14 * d * d, (Double) AviatorEvaluator.execute("pi*d*d", env), 0.001);

    assertEquals((i + pi + d + b) / 4, AviatorEvaluator.execute("(i+pi+d+b)/4", env));
    assertEquals(200, AviatorEvaluator.execute("i+100", env));
    assertEquals(0, AviatorEvaluator.execute("i%4", env));
    assertEquals(i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99, AviatorEvaluator
        .execute("i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99", env));
  }


  @Test
  public void testOperatorPrecedence() {
    assertEquals(false, AviatorEvaluator
        .execute("6.7-100>39.6 ? 5==5? 4+5:6-1 : !false ? 5-6>0&& false: 100%3<=5 || 67*40>=100"));
  }


  @Test
  public void testLogicExpression() {
    assertTrue((Boolean) AviatorEvaluator.execute("3+1==4"));
    assertTrue((Boolean) AviatorEvaluator.execute("3+1>=4"));
    assertTrue((Boolean) AviatorEvaluator.execute("3+1<=4"));
    assertFalse((Boolean) AviatorEvaluator.execute("3+1>4"));
    assertFalse((Boolean) AviatorEvaluator.execute("3+1<4"));

    assertTrue((Boolean) AviatorEvaluator.execute("100/2-50==0"));
    assertTrue((Boolean) AviatorEvaluator.execute("3-(1+2)==0"));
    assertTrue((Boolean) AviatorEvaluator.execute("3-4/2==1"));

    assertTrue((Boolean) AviatorEvaluator.execute("3<1 || -3-100<0 && !(100%3>100)"));
    assertTrue((Boolean) AviatorEvaluator.execute("3>1 || -3-100<0 && !(100%3<100)"));
    assertFalse((Boolean) AviatorEvaluator.execute("(3>1 || -3-100<0 )&& !(100%3<100)"));
    assertFalse((Boolean) AviatorEvaluator.execute("3<1 || -3-100<0 && !(100%3<100)"));
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

    assertEquals(false, AviatorEvaluator.execute("-i>=0", env));
    assertEquals(true, AviatorEvaluator.execute("-i-pi<=-100", env));
    assertEquals(true, AviatorEvaluator.execute("2*pi*10==2 * pi * 10", env));
    assertEquals(true, AviatorEvaluator.execute("pi*d*d == pi* d *d", env));

    assertEquals((i + pi + d + b) / 4 % 2 > 0, AviatorEvaluator.execute("(i+pi+d+b)/4%2>0", env));
    assertEquals(true, AviatorEvaluator.execute("(i+100)%3!=1", env));
    assertEquals(true, AviatorEvaluator.execute("i%4<=0", env));
    assertEquals(true,
        AviatorEvaluator.execute(
            "i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99 ==i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99",
            env));
  }


  @Test
  public void testSystemFunction() {
    AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    try {
      // sysdate()
      Object date = AviatorEvaluator.execute("sysdate()");
      assertNotNull(date);
      assertTrue(date instanceof Date);
      assertEquals(((Date) date).getMinutes(), new Date().getMinutes());

      // now()
      Object now = AviatorEvaluator.execute("now()");
      assertNotNull(now);
      assertTrue(now instanceof Long);
      assertEquals((Long) now, System.currentTimeMillis(), 5L);

      // rand()
      Object rand1 = AviatorEvaluator.execute("rand()");
      assertNotNull(rand1);
      assertTrue(rand1 instanceof Double);

      Object rand2 = AviatorEvaluator.execute("rand(100)");
      assertTrue(rand2 instanceof Long);
      assertTrue((Long) rand2 < 100);

      Object rand3 = AviatorEvaluator.execute("rand()");
      assertFalse(rand3.equals(rand1));
    } finally {
      AviatorEvaluator.setOption(Options.TRACE_EVAL, false);
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

    assertEquals(10, AviatorEvaluator.execute("count(a)", env));
    assertEquals(2, AviatorEvaluator.execute("count(list)", env));
    assertEquals(2, AviatorEvaluator.execute("count(set)", env));

    assertTrue((Boolean) AviatorEvaluator.execute("include(set,true)", env));
    assertTrue((Boolean) AviatorEvaluator.execute("include(set,false)", env));
    assertFalse((Boolean) AviatorEvaluator.execute("include(set,'hello')", env));
    assertFalse((Boolean) AviatorEvaluator.execute("include(set,10)", env));

    for (int i = 0; i < a.length; i++) {
      assertTrue((Boolean) AviatorEvaluator.execute("include(a,9-" + i + ")", env));
    }

    assertEquals(45, AviatorEvaluator.execute("reduce(a,+,0)", env));
    assertEquals(0, AviatorEvaluator.execute("reduce(a,*,1)", env));
    try {
      assertEquals(0, AviatorEvaluator.execute("reduce(a,/,0)", env));
      fail();
    } catch (ExpressionRuntimeException e) {
      // ignore
    }
    assertEquals(-45, AviatorEvaluator.execute("reduce(a,-,0)", env));

    assertEquals(5, AviatorEvaluator.execute("count(filter(a,seq.gt(4)))", env));
    assertEquals(4, AviatorEvaluator.execute("count(filter(a,seq.lt(4)))", env));
    assertEquals(5, AviatorEvaluator.execute("count(filter(a,seq.le(4)))", env));
    assertEquals(1, AviatorEvaluator.execute("count(filter(a,seq.eq(4)))", env));
    assertEquals(0, AviatorEvaluator.execute("count(filter(a,seq.gt(9)))", env));
    assertEquals(0, AviatorEvaluator.execute("count(filter(a,seq.nil()))", env));
    assertEquals(10, AviatorEvaluator.execute("count(filter(a,seq.exists()))", env));

    // seq.and and seq.or
    assertEquals(3,
        AviatorEvaluator.execute("count(filter(a, seq.and(seq.lt(8), seq.gt(4))))", env));
    assertEquals(4,
        AviatorEvaluator.execute("count(filter(a, seq.and(seq.lt(8), seq.ge(4))))", env));
    assertEquals(5,
        AviatorEvaluator.execute("count(filter(a, seq.and(seq.le(8), seq.ge(4))))", env));
    assertEquals(5,
        AviatorEvaluator.execute("count(filter(a, seq.or(seq.gt(8), seq.lt(4))))", env));
    assertEquals(6,
        AviatorEvaluator.execute("count(filter(a, seq.or(seq.gt(8), seq.le(4))))", env));
    assertEquals(7,
        AviatorEvaluator.execute("count(filter(a, seq.or(seq.ge(8), seq.le(4))))", env));

    assertEquals(1, AviatorEvaluator.execute("count(filter(set,seq.true()))", env));
    assertTrue((Boolean) AviatorEvaluator.execute("include(filter(set,seq.true()),true)", env));
    assertFalse((Boolean) AviatorEvaluator.execute("include(filter(set,seq.true()),false)", env));
    assertEquals(1, AviatorEvaluator.execute("count(filter(set,seq.eq(true)))", env));
    assertEquals(1, AviatorEvaluator.execute("count(filter(set,seq.false()))", env));
    assertFalse((Boolean) AviatorEvaluator.execute("include(filter(set,seq.false()),true)", env));
    assertTrue((Boolean) AviatorEvaluator.execute("include(filter(set,seq.false()),false)", env));
    assertEquals(0, AviatorEvaluator.execute("count(filter(set,seq.nil()))", env));
    assertEquals(2, AviatorEvaluator.execute("count(filter(set,seq.exists()))", env));

    assertEquals(list, AviatorEvaluator.execute("sort(list)", env));
    assertNotSame(list, AviatorEvaluator.execute("sort(list)", env));
    try {
      AviatorEvaluator.execute("sort(set)", env);
      fail();
    } catch (ExpressionRuntimeException e) {
      // ignore
    }

    assertEquals(9, a[0]);
    assertFalse(Arrays.equals(a, (Object[]) AviatorEvaluator.execute("sort(a)", env)));
    assertEquals(9, a[0]);
    Arrays.sort(a);
    assertEquals(0, a[0]);
    assertTrue(Arrays.equals(a, (Object[]) AviatorEvaluator.execute("sort(a)", env)));

    assertEquals(2, AviatorEvaluator.execute("count(map(list,string.length))", env));
    assertTrue((Boolean) AviatorEvaluator.execute("include(map(list,string.length),5)", env));
  }


  @Test
  public void testIssue2() {
    assertEquals(100000000000000000000.0 / 3.0,
        AviatorEvaluator.execute("100000000000000000000.0/3.0"));
    System.out.println(AviatorEvaluator.execute("100000000000000000000.0/3.0"));
    // assertEquals(100000000000000000000/3,
    // AviatorEvaluator.execute("100000000000000000000/3"));
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

    assertEquals("hello world aviator",
        AviatorEvaluator.execute("'hello'+' '+'world'+' '+'aviator'"));
    assertEquals(4, AviatorEvaluator.execute("string.length(\"fuck\")"));
    assertEquals(0, AviatorEvaluator.execute("string.length('')"));
    assertEquals(19, AviatorEvaluator.execute("string.length('hello'+' '+'world'+' '+'aviator')"));
    assertTrue((Boolean) AviatorEvaluator.execute("string.contains('hello','he')"));
    assertFalse((Boolean) AviatorEvaluator.execute("string.contains('hello','c')"));
    assertTrue((Boolean) AviatorEvaluator.execute("string.startsWith('hello','he')"));
    assertFalse((Boolean) AviatorEvaluator.execute("string.startsWith('hello','llo')"));
    assertFalse((Boolean) AviatorEvaluator.execute("string.endsWith('hello','he')"));
    assertTrue((Boolean) AviatorEvaluator.execute("string.endsWith('hello','llo')"));

    assertEquals("ello", AviatorEvaluator.execute("string.substring('hello',1)"));
    assertEquals("el", AviatorEvaluator.execute("string.substring('hello',1,3)"));

    // test with variable
    assertEquals("hello world aviator", AviatorEvaluator.execute("s1+' '+s3", env));
    assertEquals(19, AviatorEvaluator.execute("string.length(s1+' '+s3)", env));
    assertFalse((Boolean) AviatorEvaluator.execute("string.startsWith(s1,'fuck')", env));
    assertTrue((Boolean) AviatorEvaluator.execute("string.startsWith(s1,s1)", env));
    assertTrue((Boolean) AviatorEvaluator.execute("string.endsWith(s1+s2,s2)", env));
    assertTrue((Boolean) AviatorEvaluator.execute("string.contains(s1+s2,s1)", env));
    assertTrue((Boolean) AviatorEvaluator.execute("string.contains(s1+s2,'world')", env));
    assertFalse((Boolean) AviatorEvaluator.execute("string.contains(s1+s3,s2)", env));
    assertTrue((Boolean) AviatorEvaluator.execute("string.contains(s1+s2+s3,s2)", env));
    assertEquals("ello world", AviatorEvaluator.execute("string.substring(s1,1)", env));
    assertEquals("el", AviatorEvaluator.execute("string.substring(s1,1,3)", env));
    assertEquals("hello", ((String[]) AviatorEvaluator.exec("string.split('hello world',' ')"))[0]);
    assertEquals("world", ((String[]) AviatorEvaluator.exec("string.split('hello world',' ')"))[1]);
  }


  @Test
  public void testBitOperations() {
    assertEquals(99 | 7, AviatorEvaluator.execute("99|7"));
    assertEquals(99 | ~7, AviatorEvaluator.execute("99|~7"));
    assertEquals(99 & 7, AviatorEvaluator.execute("99&7"));
    assertEquals(99 ^ 7, AviatorEvaluator.execute("99^7"));
    assertEquals(99 << 7, AviatorEvaluator.execute("99<<7"));
    assertEquals(99 >> 7, AviatorEvaluator.execute("99>>7"));
    assertEquals(99 >>> 7, AviatorEvaluator.execute("99>>>7"));
    assertEquals(1 ^ 2 ^ 3 & 4 | 5 ^ ~2 | 5 & 4, AviatorEvaluator.execute("1^2^3&4|5^~2|5&4"));
    assertEquals((1 ^ 2 ^ 3 & 4 | 5 ^ ~2 | 5 & 4) == 100,
        AviatorEvaluator.execute("(1^2^3&4|5^~2|5&4) == 100"));
    assertEquals(
        4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? !false && true ? 1 & 4 : 0
            : 6L >> 2L * 2L / 4L ^ ~699L + 100L << 4L >> 5L >> 1000L,
        AviatorEvaluator.execute(
            "4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? (!false && true ? 1 & 4 : 0) : 6 >> 2 * 2 / 4^ ~699 + 100 << 4 >> 5 >> 1000"));

    assertEquals((99 & 7) == (99 & 7) && false, AviatorEvaluator.execute("(99&7)==(99&7)&&false "));
    assertEquals((99 | 7) != (99 | 7) || false, AviatorEvaluator.execute("(99|7)!=(99|7)||false "));
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

    assertEquals(i | j, AviatorEvaluator.execute("i|j", env));
    assertEquals(99 | k, AviatorEvaluator.execute("99|k", env));
    assertEquals(i & j, AviatorEvaluator.execute("i&j", env));
    assertEquals(99 & k, AviatorEvaluator.execute("99&k", env));
    assertEquals(i ^ j, AviatorEvaluator.execute("i^j", env));
    assertEquals(99 ^ k, AviatorEvaluator.execute("99^k", env));
    assertEquals(i | ~j, AviatorEvaluator.execute("i|~j", env));
    assertEquals(99 | ~k, AviatorEvaluator.execute("99|~k", env));
    assertEquals(j >>> i, AviatorEvaluator.execute("j>>>i", env));

    assertEquals(i ^ j ^ k & i & j & k | i | j | k & 3 & 4 | 5 & ~i,
        AviatorEvaluator.execute("i^j^k&i&j&k|i|j|k&3&4|5&~i", env));
    assertEquals(
        4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? !false && true ? 1 & 4 : 0
            : i >> j * k / i ^ ~j + k << i >> j >> 1000L,
        AviatorEvaluator.execute(
            "4 / 2 * 3 - 4 + (5 ^ 5 - 2 & 3) == 4000 ? (!false && true ? 1 & 4 : 0) :i >> j * k / i ^ ~j + k << i >> j >> 1000",
            env));

    assertEquals((i & 7) == (i & 7) && false,
        AviatorEvaluator.execute("(i & 7) == (i & 7) && false ", env));
    assertEquals((j | k) != (j | k) || false,
        AviatorEvaluator.execute("(j | k) != (j | k) || false ", env));
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

    assertEquals(0xA3, AviatorEvaluator.execute("0xA3", env));
    assertEquals(0xA3 * 0x45 + 2, AviatorEvaluator.execute("0xA3 * 0x45+2", env));
    assertEquals(0xFF == 0Xff, AviatorEvaluator.execute("0xFF==0Xff", env));
    assertEquals(~0xFF == 0Xff, AviatorEvaluator.execute("~0xFF==0Xff", env));
    assertEquals(~0xFF | k & 3 - 0X11, AviatorEvaluator.execute("~0xFF|k&3-0X11", env));
    assertEquals(0x45 > i ? 0x11 - 0344 * 5 / 7 : k / 0xFF - j * 0x45,
        AviatorEvaluator.execute("0x45>i?0x11-0344*5/7:k/0xFF-j*0x45 ", env));
  }


  @Test
  public void testBitOp_BitMap() {
    Map<String, Object> env = new HashMap<String, Object>();
    int flag = 0;
    env.put("flag", flag);
    assertEquals(false, AviatorEvaluator.execute("(flag & 0x3E0) >> 5 ==15 ", env));
    flag = flag & 0xFFFFC1F | 15 << 5;
    env.put("flag", flag);
    assertEquals(true, AviatorEvaluator.execute("(flag & 0x3E0) >> 5 ==15 ", env));

    assertEquals(false, AviatorEvaluator.execute(" (flag & 0x400) >> 10 ==1 ", env));
    flag = flag & 0xFFFFFBFF | 1 << 10;
    env.put("flag", flag);
    assertEquals(true, AviatorEvaluator.execute("(flag & 0x400) >> 10 ==1 ", env));
    assertEquals(true,
        AviatorEvaluator.execute("(flag & 0x400) >> 10 ==1 && (flag & 0x3E0) >> 5 ==15", env));
    assertEquals(false,
        AviatorEvaluator.execute("(flag & 0x400) >> 10 ==0 && (flag & 0x3E0) >> 5 ==15", env));

    assertEquals(0L, AviatorEvaluator.execute(" ((flag & 0x1800) >> 11)", env));
    flag = flag & 0xFFFFE7FF | 1 << 11;
    env.put("flag", flag);
    assertEquals(1L, AviatorEvaluator.execute(" ((flag & 0x1800) >> 11)", env));
    flag = flag & 0xFFFFE7FF | 2 << 11;
    env.put("flag", flag);
    assertEquals(2L, AviatorEvaluator.execute(" ((flag & 0x1800) >> 11)", env));
    assertEquals(flag & 0xFFFFE7FF | 3 << 11,
        AviatorEvaluator.execute(" flag & 0xFFFFE7FF | 3 << 11", env));
    flag = flag & 0xFFFFE7FF | 3 << 11;

    env.put("flag", flag);
    assertEquals(3L, AviatorEvaluator.execute(" ((flag & 0x1800) >> 11)", env));
  }


  @Test
  public void testMathFunction() {
    assertEquals(Math.pow(10, 100.0), AviatorEvaluator.exec("math.pow(10,100)"));
    assertEquals(Math.log(99), AviatorEvaluator.exec("math.log(99)"));
    assertEquals(Math.log10(99), AviatorEvaluator.exec("math.log10(99)"));
    assertEquals(Math.sin(99), AviatorEvaluator.exec("math.sin(99)"));
    assertEquals(Math.cos(99), AviatorEvaluator.exec("math.cos(99)"));
    assertEquals(Math.tan(99), AviatorEvaluator.exec("math.tan(99)"));
    assertEquals(Math.sqrt(99), AviatorEvaluator.exec("math.sqrt(99)"));
    assertEquals(Math.round(99.9), AviatorEvaluator.exec("math.round(99.9)"));
    assertEquals(Math.round(99.1), AviatorEvaluator.exec("math.round(99.1)"));
  }


  @Test
  public void testParseScientificNotations() {
    assertEquals(1e5, AviatorEvaluator.exec("1e5"));
    assertEquals(1E5, AviatorEvaluator.exec("1E5"));
    assertEquals(1E-5, AviatorEvaluator.exec("1E-5"));

    assertEquals(2e3 + 4e6, AviatorEvaluator.exec("2e3+4e6"));
    assertEquals(2e3 - 4e6, AviatorEvaluator.exec("2e3-4e6"));
    assertEquals(2e3 / 4e6, AviatorEvaluator.exec("2e3/4e6"));
    assertEquals(2e3 % 4e6, AviatorEvaluator.exec("2e3%4e6"));
  }


  @Test
  public void testParseBigNumbers() {
    assertEquals(new BigInteger("99999999999999999999999999999999"),
        AviatorEvaluator.exec("99999999999999999999999999999999"));
    assertEquals(new BigInteger("99999999999999999999999999999999"),
        AviatorEvaluator.exec("99999999999999999999999999999999N"));
    assertEquals(new BigInteger("199999999999999999999999999999998"),
        AviatorEvaluator.exec("99999999999999999999999999999999+99999999999999999999999999999999"));

    assertEquals(
        new BigDecimal("99999999999999999999999999999999.99999999",
            AviatorEvaluator.getMathContext()),
        AviatorEvaluator.exec("99999999999999999999999999999999.99999999M"));
  }


  @Test
  public void testGetVariableNames() {
    Expression expression = AviatorEvaluator.compile("b+a", true);
    assertNotNull(expression);
    List<String> vars = expression.getVariableNames();
    assertNotNull(vars);
    assertEquals(2, vars.size());
    assertTrue(vars.contains("a"));
    assertTrue(vars.contains("b"));
    assertEquals("b", vars.get(0));
    assertEquals("a", vars.get(1));

    expression = AviatorEvaluator.compile("b==a || d>3 || e+c*d/2 <= 1000", true);
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
    expression = AviatorEvaluator
        .compile("map.a>10 && list[0][1]<3 && bean.c == bean.x || bean.d == y", true);
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

    // AviatorEvaluator.setTrace(true);
    Map<String, Object> env = new HashMap<String, Object>();
    int[] a = new int[] {1, 2, 3, 4};
    int[][] b = new int[][] {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
    int[][][] c = new int[][][] {{{1, 2}, {3, 4}}, {{5, 6}, {7, 8}}};
    env.put("a", a);
    env.put("b", b);
    env.put("c", c);

    assertEquals(a[0] + b[0][0] + c[0][0][0],
        AviatorEvaluator.execute("a[0]+b[0][0]+c[0][0][0]", env));
    assertEquals(a[1] + b[0][2] * c[1][1][1],
        AviatorEvaluator.execute("a[1]+b[0][2]*c[1][1][1]", env));
    assertEquals(a[1] + b[0][2] * c[1][1][1] / (a[2] * a[1] + 100 - c[0][1][1] * b[0][1]),
        AviatorEvaluator.execute("a[1]+b[0][2]*c[1][1][1]/(a[2]*a[1]+100-c[0][1][1]*b[0][1])",
            env));
    assertEquals(c[0][1][1] > b[1][0], AviatorEvaluator.execute("c[0][1][1]>b[1][0]", env));
    assertEquals(c[0][1][1] <= b[1][0], AviatorEvaluator.execute("c[0][1] [1] <= b[1][0]", env));
    assertEquals(c[0][1][1] > b[1][0] ? a[0] : a[2],
        AviatorEvaluator.execute("c[0][1][1]>b[1][0]? a[0]:a[2]", env));
    assertEquals(b[0].length, AviatorEvaluator.execute("count(b[0])", env));
    assertEquals(6, AviatorEvaluator.execute("reduce(b[0],+,0)", env));
    Object[] rt = (Object[]) AviatorEvaluator.execute("filter(c[0][0],seq.gt(1))", env);
    assertEquals(1, rt.length);
    assertEquals(2, rt[0]);
    AviatorEvaluator.execute("map(c[1][0],println)", env);
    assertTrue((Boolean) AviatorEvaluator.execute("include(b[0],3)", env));

  }


  @Test
  public void testBigNumber() {
    // big int + long
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("a+b", 1, new BigInteger("3")));
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("a+3N", 1));
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("1+b", new BigInteger("3")));
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("3N+1"));
    assertEquals(new BigInteger("300"), AviatorEvaluator.exec("3N*100"));
    assertEquals(new BigInteger("100"), AviatorEvaluator.exec("400/4N"));
    assertEquals(new BigInteger("-3"), AviatorEvaluator.exec("a-4N", 1));

    // big int + double
    assertEquals(4.1, AviatorEvaluator.exec("a+b", 1.1, new BigInteger("3")));
    assertEquals(4.1, AviatorEvaluator.exec("a+3N", 1.1));
    assertEquals(4.1, AviatorEvaluator.exec("1.1+b", new BigInteger("3")));
    assertEquals(4.1, AviatorEvaluator.exec("3N+1.1"));
    assertEquals(300.0, AviatorEvaluator.exec("3N*100.0"));
    assertEquals(100.0, AviatorEvaluator.exec("400.0/4N"));
    assertEquals(-2.9, AviatorEvaluator.exec("a-4N", 1.1));

    // big int + big int
    assertEquals(new BigInteger("4"),
        AviatorEvaluator.exec("a+b", new BigInteger("1"), new BigInteger("3")));
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("a+3N", new BigInteger("1")));
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("1+b", new BigInteger("3")));
    assertEquals(new BigInteger("4"), AviatorEvaluator.exec("3N+1N"));
    assertEquals(new BigInteger("300"), AviatorEvaluator.exec("3N*100N"));
    assertEquals(new BigInteger("100"), AviatorEvaluator.exec("400N/4N"));
    assertEquals(new BigInteger("-3"), AviatorEvaluator.exec("a-4N", new BigInteger("1")));

    // big int + decimal
    assertEquals(new BigDecimal("4.1"),
        AviatorEvaluator.exec("a+b", new BigDecimal("1.1"), new BigInteger("3")));
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("a+3N", new BigDecimal("1")));
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("1.1M+b", new BigInteger("3")));
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("3N+1.1M"));
    assertEquals(new BigDecimal("301"), AviatorEvaluator.exec("3.01M*100N"));
    assertEquals(new BigDecimal("100"), AviatorEvaluator.exec("400M/4N"));
    assertEquals(new BigDecimal("-2.9"), AviatorEvaluator.exec("a-4N", new BigDecimal("1.1")));

    // decimal + long
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("a+b", new BigDecimal("1.1"), 3));
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("a+3", new BigDecimal("1")));
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("1.1M+b", 3));
    assertEquals(new BigDecimal("4.1"), AviatorEvaluator.exec("3+1.1M"));
    assertEquals(new BigDecimal("301"), AviatorEvaluator.exec("3.01M*100"));
    assertEquals(new BigDecimal("100"), AviatorEvaluator.exec("400M/4"));
    assertEquals(new BigDecimal("-2.9"), AviatorEvaluator.exec("a-4", new BigDecimal("1.1")));
    // decimal + double
    assertEquals(4.1, AviatorEvaluator.exec("a+b", 1.1, new BigDecimal("3")));
    assertEquals(4.1, AviatorEvaluator.exec("a+3.0M", 1.1));
    assertEquals(4.1, AviatorEvaluator.exec("1.1+b", new BigDecimal("3")));
    assertEquals(4.1, AviatorEvaluator.exec("3.00M+1.1"));
    assertEquals(300.0, AviatorEvaluator.exec("3M*100.0"));
    assertEquals(100.0, AviatorEvaluator.exec("400.0/4M"));
    assertEquals(-2.9, AviatorEvaluator.exec("a-4.00M", 1.1));
  }


  @Test
  public void testBigNumberNegative() {
    assertEquals(new BigInteger("-1000000000000000000000000000000000"),
        AviatorEvaluator.exec("-a", new BigInteger("1000000000000000000000000000000000")));
    assertEquals(new BigDecimal("9999999999999999999999999999999999999.99999999999"),
        AviatorEvaluator.exec("-a",
            new BigDecimal("-9999999999999999999999999999999999999.99999999999")));
    assertEquals(new BigDecimal("9999999999999999999.999999999999"),
        AviatorEvaluator.exec("-(-9999999999999999999.999999999999M)"));
    assertEquals(new BigInteger("9999999999999999999"),
        AviatorEvaluator.exec("-(-9999999999999999999N)"));
  }


  @Test
  public void testBigNumberBitOperations() {
    assertEquals(
        new BigInteger("1000000000000000000000000000000000")
            .xor(new BigInteger("9999999999999999999999")),
        AviatorEvaluator.exec("a^b", new BigInteger("1000000000000000000000000000000000"),
            new BigInteger("9999999999999999999999")));
    assertEquals(
        new BigInteger("1000000000000000000000000000000000")
            .and(new BigInteger("9999999999999999999999")),
        AviatorEvaluator.exec("a&b", new BigInteger("1000000000000000000000000000000000"),
            new BigInteger("9999999999999999999999")));
    assertEquals(
        new BigInteger("1000000000000000000000000000000000")
            .or(new BigInteger("9999999999999999999999")),
        AviatorEvaluator.exec("a|b", new BigInteger("1000000000000000000000000000000000"),
            new BigInteger("9999999999999999999999")));
    assertEquals(new BigInteger("1000000000000000000000000000000000").shiftLeft(2),
        AviatorEvaluator.exec("a<<2", new BigInteger("1000000000000000000000000000000000")));
    assertEquals(new BigInteger("1000000000000000000000000000000000").shiftRight(2),
        AviatorEvaluator.exec("a>>2", new BigInteger("1000000000000000000000000000000000")));
    assertEquals(new BigInteger("1000000000000000000000000000000000").shiftRight(2),
        AviatorEvaluator.exec("a>>>2", new BigInteger("1000000000000000000000000000000000")));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testDecimalBitAnd() {
    AviatorEvaluator.exec("3M< & 2M");
  }


  @Test
  public void testAlwaysUseDoubleAsDecimal() {
    AviatorEvaluator.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, true);
    try {
      Object val = AviatorEvaluator.execute("3.2");
      assertTrue(val instanceof BigDecimal);
      assertEquals(new BigDecimal("3.2"), val);

      val = AviatorEvaluator.execute("3.2 + 4.3");
      assertTrue(val instanceof BigDecimal);
      assertEquals(new BigDecimal("7.5"), val);

      Map<String, Object> env = new HashMap<String, Object>();
      env.put("a", new BigDecimal("2.1"));
      env.put("b", 4);
      val = AviatorEvaluator.execute("3.2 + a * b ", env);
      assertTrue(val instanceof BigDecimal);
      assertEquals(new BigDecimal("11.6"), val);
    } finally {
      AviatorEvaluator.setOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL, false);
    }
  }


  @Test
  public void testOtherFunction() {
    // AviatorEvaluator.setOptimize(AviatorEvaluator.EVAL);
    // System.setProperty("aviator.asm.trace","true");
    assertTrue((Boolean) AviatorEvaluator
        .execute("'A' == 'A' || 'B' == 'B' && 'ABCD' == t &&  'A' == 'A'"));

  }

  @Test
  public void testSeqFilterListWithProperty() {
    List<User> users = new ArrayList<>(3);
    users.add(new User(1L, 25, "张三"));
    users.add(new User(2L, 26, "李四"));
    users.add(new User(3L, 27, "王五"));
    Map<String, Object> env = new HashMap<>();
    env.put("data", users);
    Object result =
        AviatorEvaluator.execute("filter(data,seq.and(seq.gt(25,'age'),seq.eq('李四','name')))", env);
    List list = (List) result;
    assertEquals(1, list.size());
    for (Object o : list) {
      User user = (User) o;
      assertEquals("李四", user.getName());
      assertTrue(user.getAge() > 25);
    }
  }

  @Test
  public void testSeqFilterMapWithProperty() {
    Map<Long, User> idUserMap = new HashMap<>(3);
    idUserMap.put(1L, new User(1L, 25, "张三"));
    idUserMap.put(2L, new User(2L, 26, "李四"));
    idUserMap.put(3L, new User(3L, 27, "王五"));
    Map<String, Object> env = new HashMap<>();
    env.put("data", idUserMap);
    Object result = AviatorEvaluator
        .execute("filter(data,seq.and(seq.gt(25,'value.age'),seq.eq('李四','value.name')))", env);
    Map map = (Map) result;
    assertEquals(1, map.size());
    for (Object o : map.values()) {
      User user = (User) o;
      assertEquals("李四", user.getName());
      assertTrue(user.getAge() > 25);
    }
  }

  public static class User {
    private Long id;
    private Integer age;
    private String name;

    public User(Long id, Integer age, String name) {
      this.id = id;
      this.age = age;
      this.name = name;
    }

    public Long getId() {
      return id;
    }

    public void setId(Long id) {
      this.id = id;
    }

    public Integer getAge() {
      return age;
    }

    public void setAge(Integer age) {
      this.age = age;
    }

    public String getName() {
      return name;
    }

    public void setName(String name) {
      this.name = name;
    }

    @Override
    public String toString() {
      return "User{" + "id=" + id + ", age=" + age + ", name='" + name + '\'' + '}';
    }
  }
}
