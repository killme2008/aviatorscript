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
package com.googlecode.aviator.test.function;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.exception.ExpressionRuntimeException;


public class FunctionTest {
    @Test
    public void testArithmeticExpression() {
        assertEquals(1 + 2 + 3 + 4 + 5 + 6 + 7 + 8 + 9 + 10, AviatorEvaluator.execute("1+2+3+4+5+6+7+8+9+10"));
        assertEquals(0, AviatorEvaluator.execute("1+2-3"));
        assertEquals(120, AviatorEvaluator.execute("1*2*3*4*5"));
        assertEquals(-4, AviatorEvaluator.execute("1-2-3"));
        assertEquals(2, AviatorEvaluator.execute("1-(2-3)"));

        assertEquals(50, AviatorEvaluator.execute("100/2"));
        assertEquals(33, AviatorEvaluator.execute("100/3"));

        assertEquals(-49, AviatorEvaluator.execute("1-100/2"));
        assertEquals(51, AviatorEvaluator.execute("1+100/2"));
        assertEquals(6 - (4 / 2 - (4 + 5)) * 2 + 100 / (2 + 1) * 20 - 5 * 5 * 5 + (6 + 1) / (2 - 3 / (1 + 1)),
            AviatorEvaluator.execute("6-(4/2-(4+5))*2+100/(2+1)*20-5*5*5+(6+1)/(2-3/(1+1))"));

        assertEquals(62.8, AviatorEvaluator.execute("2*3.14*10"));
        assertEquals(96.3, AviatorEvaluator.execute("100.3-4"));
        assertEquals(-96.3, AviatorEvaluator.execute("4-100.3"));
        assertEquals(100.3 / 4 - (4.0 / 2 + 5), AviatorEvaluator.execute("100.3/4-(4.0/2+5)"));

        assertEquals(1, AviatorEvaluator.execute("100%3"));
        assertEquals(0, AviatorEvaluator.execute("1-100%3"));
        assertEquals(100 % 3 * 4.2 + (37 + 95) / (6 * 3 - 18.0), (Double) AviatorEvaluator
            .execute("100%3*4.2+(37+95)/(6*3-18.0)"), 0.0001);
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

        assertEquals(-100, AviatorEvaluator.execute("-i", env));
        assertEquals(-103.4, AviatorEvaluator.execute("-i-pi", env));
        assertEquals(2 * 3.14 * 10, (Double) AviatorEvaluator.execute("2*pi*10", env), 0.001);
        assertEquals(3.14 * d * d, (Double) AviatorEvaluator.execute("pi*d*d", env), 0.001);

        assertEquals((i + pi + d + b) / 4, AviatorEvaluator.execute("(i+pi+d+b)/4", env));
        assertEquals(200, AviatorEvaluator.execute("i+100", env));
        assertEquals(0, AviatorEvaluator.execute("i%4", env));
        assertEquals(i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99, AviatorEvaluator.execute(
            "i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99", env));
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
        assertEquals(
            true,
            AviatorEvaluator
                .execute(
                    "i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99 ==i * pi + (d * b - 199) / (1 - d * pi) - (2 + 100 - i / pi) % 99",
                    env));
    }


    @Test
    public void testSystemFunction() {
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

        Object rand2 = AviatorEvaluator.execute("rand()");
        assertFalse(rand1.equals(rand2));

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
        }
        catch (ExpressionRuntimeException e) {
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
        }
        catch (ExpressionRuntimeException e) {
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
    public void testStringFunction() {
        String s1 = "hello world";
        String s2 = "just for fun";
        String s3 = "aviator";

        Map<String, Object> env = new HashMap<String, Object>();
        env.put("s1", s1);
        env.put("s2", s2);
        env.put("s3", s3);

        assertEquals("hello world aviator", AviatorEvaluator.execute("'hello'+' '+'world'+' '+'aviator'"));
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
    }


    @Test
    public void testMathFunction() {

    }


    @Test
    public void testOtherFunction() {
        // AviatorEvaluator.setOptimize(AviatorEvaluator.EVAL);
        // System.setProperty("aviator.asm.trace","true");
        assertTrue((Boolean) AviatorEvaluator.execute("'A' == 'A' || 'B' == 'B' && 'ABCD' == t &&  'A' == 'A'"));

    }
}
