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
package com.googlecode.aviator.runtime.type;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.utils.Env;


public class AviatorNumberUnitTest {

  @Test
  public void testCompareWithNumber() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    AviatorNumber b = AviatorNumber.valueOf(2000);
    assertTrue(a.compare(b, null) < 0);
    assertTrue(b.compare(a, null) > 0);

    AviatorNumber c = AviatorNumber.valueOf(3.2f);
    AviatorNumber d = AviatorNumber.valueOf(-0.3d);
    assertTrue(c.compare(d, null) > 0);
    assertTrue(d.compare(c, null) < 0);

    a = AviatorNumber.valueOf(1000);
    b = AviatorNumber.valueOf(1000);

    assertEquals(0, a.compare(b, null));
    assertEquals(0, b.compare(a, null));
    assertTrue(a.compare(c, null) > 0);
    assertTrue(b.compare(d, null) > 0);
    assertTrue(d.compare(c, null) < 0);
    assertTrue(d.compare(a, null) < 0);
    assertTrue(d.compare(b, null) < 0);
  }


  @Test
  public void testCompareWithJavaType() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    AviatorJavaType longType = new AviatorJavaType("longType");
    AviatorJavaType byteType = new AviatorJavaType("byteType");
    AviatorJavaType shortType = new AviatorJavaType("shortType");
    AviatorJavaType intType = new AviatorJavaType("intType");

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("byteType", (byte) 3);
    env.put("shortType", (short) 1000);
    env.put("intType", 500);
    env.put("longType", (long) 2000);

    assertTrue(a.compare(longType, env) < 0);
    assertTrue(a.compare(byteType, env) > 0);
    assertEquals(0, a.compare(shortType, env));
    assertTrue(a.compare(intType, env) > 0);

    AviatorJavaType floatType = new AviatorJavaType("floatType");
    AviatorJavaType doubleType = new AviatorJavaType("doubleType");
    env.put("floatType", 1000.1f);
    env.put("doubleType", 999.9d);
    assertTrue(a.compare(floatType, env) < 0);

    assertTrue(a.compare(doubleType, env) > 0);
    assertEquals(1, a.compare(new AviatorJavaType("unknow"), env));
  }


  private Map<String, Object> createEnvWith(String name, Object obj) {
    Map<String, Object> env = new HashMap<String, Object>();
    if (name != null) {
      env.put(name, obj);
    }
    env.put("true", Boolean.TRUE);
    env.put("false", Boolean.FALSE);
    return env;
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareWithJavaString() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    AviatorJavaType s = new AviatorJavaType("s");
    a.compare(s, this.createEnvWith("s", "hello"));

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareWithString() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    AviatorString s = new AviatorString("hello");
    a.compare(s, null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddWithOtherType1() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.add(AviatorBoolean.valueOf(Boolean.TRUE), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddWithOtherType2() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.add(new AviatorJavaType("true"), this.createEnvWith(null, null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testSubWithOtherType1() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.sub(AviatorBoolean.valueOf(Boolean.TRUE), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testSubWithOtherType2() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.sub(new AviatorJavaType("true"), this.createEnvWith(null, null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testMultWithOtherType1() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.mult(AviatorBoolean.valueOf(Boolean.TRUE), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testMultWithOtherType2() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.mult(new AviatorJavaType("true"), this.createEnvWith(null, null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testDivWithOtherType1() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.div(AviatorBoolean.valueOf(Boolean.TRUE), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testDivWithOtherType2() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.div(new AviatorJavaType("true"), this.createEnvWith(null, null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testModWithOtherType1() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.mod(AviatorBoolean.valueOf(Boolean.TRUE), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testModWithOtherType2() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    a.mod(new AviatorJavaType("true"), this.createEnvWith(null, null));
  }


  @Test
  public void testAddWithString() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    AviatorString s = new AviatorString("hello");
    assertEquals("1000hello", a.add(s, null).getValue(null));
    assertEquals("1000hello", a.add(s, null).getValue(null));
    assertEquals(AviatorType.String, a.add(s, null).getAviatorType());
  }


  @Test
  public void testAddWithJavaString() {
    AviatorNumber a = AviatorNumber.valueOf(1000);
    AviatorJavaType s = new AviatorJavaType("s");
    assertEquals("1000hello", a.add(s, this.createEnvWith("s", "hello")).getValue(null));
    assertEquals("1000hello", a.add(s, this.createEnvWith("s", "hello")).getValue(null));
    assertEquals(AviatorType.String, a.add(s, this.createEnvWith("s", "hello")).getAviatorType());
  }


  @Test
  public void testAddWithJavaType() {
    this.doArithOperationWithJavaType(OperatorType.Add);
  }


  @Test
  public void testSubWithJavaType() {

    this.doArithOperationWithJavaType(OperatorType.Sub);
  }


  @Test
  public void testMultWithJavaType() {

    this.doArithOperationWithJavaType(OperatorType.Mult);
  }


  @Test
  public void testDivWithJavaType() {

    this.doArithOperationWithJavaType(OperatorType.Div);
  }


  @Test
  public void testModWithJavaType() {

    this.doArithOperationWithJavaType(OperatorType.Mod);
  }


  public void doArithOperationWithJavaType(OperatorType operatorType) {
    AviatorNumber a = AviatorNumber.valueOf(1000);

    AviatorJavaType byteType = new AviatorJavaType("byteType");
    AviatorJavaType shortType = new AviatorJavaType("shortType");
    AviatorJavaType intType = new AviatorJavaType("intType");
    AviatorJavaType longType = new AviatorJavaType("longType");
    AviatorJavaType floatType = new AviatorJavaType("floatType");
    AviatorJavaType doubleType = new AviatorJavaType("doubleType");

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("byteType", (byte) 4);
    env.put("shortType", (short) 4);
    env.put("intType", 4);
    env.put("longType", (long) 4);
    env.put("floatType", 4.3f);
    env.put("doubleType", 4.3d);
    switch (operatorType) {
      case Add:
        a = AviatorNumber.valueOf(1000);
        assertEquals(1004L, a.add(byteType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(1004L, a.add(shortType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(1004L, a.add(intType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(1004L, a.add(longType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(1004.3, a.add(doubleType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(1004.3, a.add(floatType, env).getValue(null));
        break;
      case Sub:
        a = AviatorNumber.valueOf(1000);
        assertEquals(996L, a.sub(byteType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(996L, a.sub(shortType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(996L, a.sub(intType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(996L, a.sub(longType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(995.7d, a.sub(doubleType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(995.7d, a.sub(floatType, env).getValue(null));
        break;

      case Mod:
        a = AviatorNumber.valueOf(1000);
        // 1000 4 4.3
        assertEquals(0L, a.mod(byteType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(0L, a.mod(shortType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(0L, a.mod(intType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(0L, a.mod(longType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(2.4000, (Double) a.mod(doubleType, env).getValue(null), 0.001);
        a = AviatorNumber.valueOf(1000);
        assertEquals(2.4000, (Double) a.mod(floatType, env).getValue(null), 0.001);
        break;
      case Mult:
        a = AviatorNumber.valueOf(1000);
        assertEquals(4000L, a.mult(byteType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(4000L, a.mult(shortType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(4000L, a.mult(intType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(4000L, a.mult(longType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(4300.0, (Double) a.mult(doubleType, env).getValue(null), 0.001);
        a = AviatorNumber.valueOf(1000);
        assertEquals(4300.0, (Double) a.mult(floatType, env).getValue(null), 0.001);
        break;
      case Div:
        a = AviatorNumber.valueOf(1000);
        assertEquals(250L, a.div(byteType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(250L, a.div(shortType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(250L, a.div(intType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(250L, a.div(longType, env).getValue(null));
        a = AviatorNumber.valueOf(1000);
        assertEquals(232.558139, (Double) a.div(doubleType, env).getValue(null), 0.001);
        a = AviatorNumber.valueOf(1000);
        assertEquals(232.558139, (Double) a.div(floatType, env).getValue(null), 0.001);
        break;

    }
  }


  @Test
  public void testAddWithNumber() {
    this.testArthOperationWithNumber(OperatorType.Add);
  }


  @Test
  public void testSubWithNumber() {
    this.testArthOperationWithNumber(OperatorType.Sub);
  }


  @Test
  public void testMultWithNumber() {
    this.testArthOperationWithNumber(OperatorType.Mult);
  }


  @Test
  public void testDivWithNumber() {
    this.testArthOperationWithNumber(OperatorType.Div);
  }


  @Test
  public void testModWithNumber() {
    this.testArthOperationWithNumber(OperatorType.Mod);
  }

  private enum OperatorType {
    Add, Sub, Mod, Mult, Div
  }

  AviatorNumber a = AviatorNumber.valueOf(3.3f);
  AviatorNumber b = AviatorNumber.valueOf(3);
  AviatorNumber c = AviatorNumber.valueOf(1000);
  AviatorNumber d = AviatorNumber.valueOf(4.3d);
  AviatorNumber e = AviatorNumber.valueOf(new BigInteger(String.valueOf(Long.MAX_VALUE) + "1"));
  AviatorNumber f =
      AviatorNumber.valueOf(new BigDecimal(String.valueOf(Long.MAX_VALUE) + "1.1001"));


  public void testArthOperationWithNumber(OperatorType operatorType) {
    Env env = TestUtils.getTestEnv();
    switch (operatorType) {
      case Add:
        this.reset();
        assertEquals(6.3, this.a.add(this.b, env).getValue(env));
        this.reset();
        assertEquals(6.3, this.b.add(this.a, env).getValue(env));
        this.reset();
        assertEquals(7.6, this.a.add(this.d, env).getValue(env));
        this.reset();
        assertEquals(7.6, this.d.add(this.a, env).getValue(env));
        this.reset();
        assertEquals(1003, this.b.add(this.c, env).getValue(env));
        this.reset();
        assertEquals(1003, this.c.add(this.b, env).getValue(env));
        this.reset();
        assertEquals(7.3, this.b.add(this.d, env).getValue(env));
        this.reset();
        assertEquals(7.3, this.d.add(this.b, env).getValue(env));

        assertEquals(new BigInteger("92233720368547758074"), this.b.add(this.e, env).getValue(env));
        assertEquals(new BigInteger("92233720368547759071"), this.c.add(this.e, env).getValue(env));
        assertEquals(9.223372036854776E18, this.a.add(this.e, env).getValue(env));

        assertEquals(new BigDecimal("92233720368547758074.1001"),
            this.b.add(this.f, env).getValue(env));
        assertEquals(new BigDecimal("92233720368547759071.1001"),
            this.c.add(this.f, env).getValue(env));
        assertEquals(9.223372036854776E19, this.a.add(this.f, env).getValue(env));
        break;
      case Sub:
        this.reset();
        assertEquals(0.3, this.a.sub(this.b, env).getValue(env));
        this.reset();
        assertEquals(-0.3, this.b.sub(this.a, env).getValue(env));
        this.reset();
        assertEquals(-1.0, this.a.sub(this.d, env).getValue(env));
        this.reset();
        assertEquals(1.0, this.d.sub(this.a, env).getValue(env));
        this.reset();
        assertEquals(-997, this.b.sub(this.c, env).getValue(env));
        this.reset();
        assertEquals(997, this.c.sub(this.b, env).getValue(env));
        this.reset();
        assertEquals(-1.3, this.b.sub(this.d, env).getValue(env));
        this.reset();
        assertEquals(1.3, this.d.sub(this.b, env).getValue(env));

        assertEquals(new BigInteger("-92233720368547758068"),
            this.b.sub(this.e, env).getValue(env));
        assertEquals(new BigInteger("-92233720368547757071"),
            this.c.sub(this.e, env).getValue(env));
        assertEquals(-9.223372036854776E19, this.a.sub(this.e, env).getValue(env));

        assertEquals(new BigDecimal("-92233720368547758068.1001"),
            this.b.sub(this.f, env).getValue(env));
        assertEquals(new BigDecimal("-92233720368547757071.1001"),
            this.c.sub(this.f, env).getValue(env));
        assertEquals(-9.223372036854776E19, this.a.sub(this.f, env).getValue(env));
        break;
      case Mult:
        this.reset();
        assertEquals(9.9, this.a.mult(this.b, env).getValue(env));
        this.reset();
        assertEquals(9.9, this.b.mult(this.a, env).getValue(env));
        this.reset();
        assertEquals(14.19, this.a.mult(this.d, env).getValue(env));
        this.reset();
        assertEquals(14.19, this.d.mult(this.a, env).getValue(env));
        this.reset();
        assertEquals(3000, this.b.mult(this.c, env).getValue(env));
        this.reset();
        assertEquals(3000, this.c.mult(this.b, env).getValue(env));
        this.reset();
        assertEquals(12.9, this.b.mult(this.d, env).getValue(env));
        this.reset();
        assertEquals(12.9, this.d.mult(this.b, env).getValue(env));

        assertEquals(new BigInteger("276701161105643274213"),
            this.b.mult(this.e, env).getValue(env));
        assertEquals(new BigInteger("92233720368547758071000"),
            this.c.mult(this.e, env).getValue(env));
        assertEquals(3.043712772162076E20, this.a.mult(this.e, env).getValue(env));

        assertEquals(new BigDecimal("276701161105643274213.3003"),
            this.b.mult(this.f, env).getValue(env));
        assertEquals(new BigDecimal("92233720368547758071100.1000"),
            this.c.mult(this.f, env).getValue(env));
        assertEquals(3.043712772162076E20, this.a.mult(this.f, env).getValue(env));
        break;

      case Div:
        this.reset();
        // 3.3 3 1000 4.3
        assertEquals(1.1, (Double) this.a.div(this.b, env).getValue(env), 0.001);
        this.reset();
        assertEquals(0.90909090, (Double) this.b.div(this.a, env).getValue(env), 0.001);
        this.reset();
        assertEquals(0.76744, (Double) this.a.div(this.d, env).getValue(env), 0.001);
        this.reset();
        assertEquals(1.30303030, (Double) this.d.div(this.a, env).getValue(env), 0.001);
        this.reset();
        assertEquals(0, this.b.div(this.c, env).getValue(env));
        this.reset();
        assertEquals(333, this.c.div(this.b, env).getValue(env));
        this.reset();
        assertEquals(0.6976744, (Double) this.b.div(this.d, env).getValue(env), 0.001);
        this.reset();
        assertEquals(1.433333333, (Double) this.d.div(this.b, env).getValue(env), 0.001);

        assertEquals(new BigInteger("30744573456182586023"), this.e.div(this.b, env).getValue(env));
        assertEquals(new BigInteger("92233720368547758"), this.e.div(this.c, env).getValue(env));
        assertEquals(2.794961223289326E19, this.e.div(this.a, env).getValue(env));

        assertEquals(new BigDecimal("3.252606517456513302336211867796323E-20"),
            this.b.div(this.f, env).getValue(env));
        assertEquals(new BigDecimal("1.084202172485504434112070622598774E-17"),
            this.c.div(this.f, env).getValue(env));
        assertEquals(3.577867169202164E-20, this.a.div(this.f, env).getValue(env));
        break;
      case Mod:
        this.reset();
        assertEquals(0.3, (Double) this.a.mod(this.b, env).getValue(env), 0.001);
        this.reset();
        assertEquals(3.0, (Double) this.b.mod(this.a, env).getValue(env), 0.001);
        this.reset();
        assertEquals(3.3, (Double) this.a.mod(this.d, env).getValue(env), 0.001);
        this.reset();
        assertEquals(1.0, (Double) this.d.mod(this.a, env).getValue(env), 0.001);
        this.reset();
        assertEquals(3, this.b.mod(this.c, env).getValue(env));
        this.reset();
        assertEquals(1, this.c.mod(this.b, env).getValue(env));
        this.reset();
        assertEquals(3.0, (Double) this.b.mod(this.d, env).getValue(env), 0.001);
        this.reset();
        assertEquals(1.3, (Double) this.d.mod(this.b, env).getValue(env), 0.001);

        assertEquals(new BigInteger("2"), this.e.mod(this.b, env).getValue(env));
        assertEquals(new BigInteger("71"), this.e.mod(this.c, env).getValue(env));
        assertEquals(0.0, this.e.mod(this.a, env).getValue(env));

        assertEquals(new BigDecimal("3"), this.b.mod(this.f, env).getValue(env));
        assertEquals(new BigDecimal("1000"), this.c.mod(this.f, env).getValue(env));
        assertEquals(3.3, this.a.mod(this.f, env).getValue(env));

        assertEquals(new BigDecimal("2.1001"), this.f.mod(this.b, env).getValue(env));
        assertEquals(new BigDecimal("71.1001"), this.f.mod(this.c, env).getValue(env));
        assertEquals(0.0, this.f.mod(this.a, env).getValue(env));
        break;
    }
  }


  private void reset() {
    this.a = AviatorNumber.valueOf(3.3f);
    this.b = AviatorNumber.valueOf(3);
    this.c = AviatorNumber.valueOf(1000);
    this.d = AviatorNumber.valueOf(4.3d);
    this.e = AviatorNumber.valueOf(new BigInteger(String.valueOf(Long.MAX_VALUE) + "1"));
    this.f = AviatorNumber.valueOf(new BigDecimal(String.valueOf(Long.MAX_VALUE) + "1.1001"));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testNot() {
    AviatorNumber.valueOf(3).not(null);
  }


  @Test
  public void testNeg() {
    AviatorNumber n = AviatorNumber.valueOf(3.3);
    assertEquals(-3.3, n.neg(null).getValue(null));

    n = AviatorNumber.valueOf(3);
    assertEquals(-3L, n.neg(null).getValue(null));
  }

}
