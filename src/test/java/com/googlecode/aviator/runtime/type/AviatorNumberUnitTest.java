/**
 *  Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.type;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.googlecode.aviator.exception.ExpressionRuntimeException;


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
        a.compare(s, createEnvWith("s", "hello"));

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
        a.add(new AviatorJavaType("true"), createEnvWith(null, null));
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testSubWithOtherType1() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.sub(AviatorBoolean.valueOf(Boolean.TRUE), null);
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testSubWithOtherType2() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.sub(new AviatorJavaType("true"), createEnvWith(null, null));
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testMultWithOtherType1() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.mult(AviatorBoolean.valueOf(Boolean.TRUE), null);
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testMultWithOtherType2() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.mult(new AviatorJavaType("true"), createEnvWith(null, null));
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testDivWithOtherType1() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.div(AviatorBoolean.valueOf(Boolean.TRUE), null);
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testDivWithOtherType2() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.div(new AviatorJavaType("true"), createEnvWith(null, null));
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testModWithOtherType1() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.mod(AviatorBoolean.valueOf(Boolean.TRUE), null);
    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testModWithOtherType2() {
        AviatorNumber a = AviatorNumber.valueOf(1000);
        a.mod(new AviatorJavaType("true"), createEnvWith(null, null));
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
        assertEquals("1000hello", a.add(s, createEnvWith("s", "hello")).getValue(null));
        assertEquals("1000hello", a.add(s, createEnvWith("s", "hello")).getValue(null));
        assertEquals(AviatorType.String, a.add(s, createEnvWith("s", "hello")).getAviatorType());
    }


    @Test
    public void testAddWithJavaType() {
        doArithOperationWithJavaType(OperatorType.Add);
    }


    @Test
    public void testSubWithJavaType() {

        doArithOperationWithJavaType(OperatorType.Sub);
    }


    @Test
    public void testMultWithJavaType() {

        doArithOperationWithJavaType(OperatorType.Mult);
    }


    @Test
    public void testDivWithJavaType() {

        doArithOperationWithJavaType(OperatorType.Div);
    }


    @Test
    public void testModWithJavaType() {

        doArithOperationWithJavaType(OperatorType.Mod);
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

            assertEquals(1004L, a.add(byteType, env).getValue(null));
            assertEquals(1004L, a.add(shortType, env).getValue(null));
            assertEquals(1004L, a.add(intType, env).getValue(null));
            assertEquals(1004L, a.add(longType, env).getValue(null));
            assertEquals(1004.3, a.add(doubleType, env).getValue(null));
            assertEquals(1004.3, a.add(floatType, env).getValue(null));
            break;
        case Sub:
            assertEquals(996L, a.sub(byteType, env).getValue(null));
            assertEquals(996L, a.sub(shortType, env).getValue(null));
            assertEquals(996L, a.sub(intType, env).getValue(null));
            assertEquals(996L, a.sub(longType, env).getValue(null));
            assertEquals(995.7d, a.sub(doubleType, env).getValue(null));
            assertEquals(995.7d, a.sub(floatType, env).getValue(null));
            break;

        case Mod:
            // 1000 4 4.3
            assertEquals(0L, a.mod(byteType, env).getValue(null));
            assertEquals(0L, a.mod(shortType, env).getValue(null));
            assertEquals(0L, a.mod(intType, env).getValue(null));
            assertEquals(0L, a.mod(longType, env).getValue(null));
            assertEquals(2.4000, (Double) a.mod(doubleType, env).getValue(null), 0.001);
            assertEquals(2.4000, (Double) a.mod(floatType, env).getValue(null), 0.001);
            break;
        case Mult:
            assertEquals(4000L, a.mult(byteType, env).getValue(null));
            assertEquals(4000L, a.mult(shortType, env).getValue(null));
            assertEquals(4000L, a.mult(intType, env).getValue(null));
            assertEquals(4000L, a.mult(longType, env).getValue(null));
            assertEquals(4300.0, (Double) a.mult(doubleType, env).getValue(null), 0.001);
            assertEquals(4300.0, (Double) a.mult(floatType, env).getValue(null), 0.001);
            break;
        case Div:
            assertEquals(250L, a.div(byteType, env).getValue(null));
            assertEquals(250L, a.div(shortType, env).getValue(null));
            assertEquals(250L, a.div(intType, env).getValue(null));
            assertEquals(250L, a.div(longType, env).getValue(null));
            assertEquals(232.558139, (Double) a.div(doubleType, env).getValue(null), 0.001);
            assertEquals(232.558139, (Double) a.div(floatType, env).getValue(null), 0.001);
            break;

        }
    }


    @Test
    public void testAddWithNumber() {
        testArthOperationWithNumber(OperatorType.Add);
    }


    @Test
    public void testSubWithNumber() {
        testArthOperationWithNumber(OperatorType.Sub);
    }


    @Test
    public void testMultWithNumber() {
        testArthOperationWithNumber(OperatorType.Mult);
    }


    @Test
    public void testDivWithNumber() {
        testArthOperationWithNumber(OperatorType.Div);
    }


    @Test
    public void testModWithNumber() {
        testArthOperationWithNumber(OperatorType.Mod);
    }

    private enum OperatorType {
        Add,
        Sub,
        Mod,
        Mult,
        Div
    }


    public void testArthOperationWithNumber(OperatorType operatorType) {
        AviatorNumber a = AviatorNumber.valueOf(3.3f);
        AviatorNumber b = AviatorNumber.valueOf(3);
        AviatorNumber c = AviatorNumber.valueOf(1000);
        AviatorNumber d = AviatorNumber.valueOf(4.3d);
        switch (operatorType) {
        case Add:
            assertEquals(6.3, a.add(b, null).getValue(null));
            assertEquals(6.3, b.add(a, null).getValue(null));

            assertEquals(7.6, a.add(d, null).getValue(null));
            assertEquals(7.6, d.add(a, null).getValue(null));

            assertEquals(1003, b.add(c, null).getValue(null));
            assertEquals(1003, c.add(b, null).getValue(null));

            assertEquals(7.3, b.add(d, null).getValue(null));
            assertEquals(7.3, d.add(b, null).getValue(null));
            break;
        case Sub:
            assertEquals(0.3, a.sub(b, null).getValue(null));
            assertEquals(-0.3, b.sub(a, null).getValue(null));

            assertEquals(-1.0, a.sub(d, null).getValue(null));
            assertEquals(1.0, d.sub(a, null).getValue(null));

            assertEquals(-997, b.sub(c, null).getValue(null));
            assertEquals(997, c.sub(b, null).getValue(null));

            assertEquals(-1.3, b.sub(d, null).getValue(null));
            assertEquals(1.3, d.sub(b, null).getValue(null));
            break;
        case Mult:
            assertEquals(9.9, a.mult(b, null).getValue(null));
            assertEquals(9.9, b.mult(a, null).getValue(null));

            assertEquals(14.19, a.mult(d, null).getValue(null));
            assertEquals(14.19, d.mult(a, null).getValue(null));

            assertEquals(3000, b.mult(c, null).getValue(null));
            assertEquals(3000, c.mult(b, null).getValue(null));

            assertEquals(12.9, b.mult(d, null).getValue(null));
            assertEquals(12.9, d.mult(b, null).getValue(null));
            break;

        case Div:
            // 3.3 3 1000 4.3
            assertEquals(1.1, (Double) a.div(b, null).getValue(null), 0.001);
            assertEquals(0.90909090, (Double) b.div(a, null).getValue(null), 0.001);

            assertEquals(0.76744, (Double) a.div(d, null).getValue(null), 0.001);
            assertEquals(1.30303030, (Double) d.div(a, null).getValue(null), 0.001);

            assertEquals(0, b.div(c, null).getValue(null));
            assertEquals(333, c.div(b, null).getValue(null));

            assertEquals(0.6976744, (Double) b.div(d, null).getValue(null), 0.001);
            assertEquals(1.433333333, (Double) d.div(b, null).getValue(null), 0.001);
            break;
        case Mod:
            assertEquals(0.3, (Double) a.mod(b, null).getValue(null), 0.001);
            assertEquals(3.0, (Double) b.mod(a, null).getValue(null), 0.001);

            assertEquals(3.3, (Double) a.mod(d, null).getValue(null), 0.001);
            assertEquals(1.0, (Double) d.mod(a, null).getValue(null), 0.001);

            assertEquals(3, b.mod(c, null).getValue(null));
            assertEquals(1, c.mod(b, null).getValue(null));

            assertEquals(3.0, (Double) b.mod(d, null).getValue(null), 0.001);
            assertEquals(1.3, (Double) d.mod(b, null).getValue(null), 0.001);
            break;

        }

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
