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
package com.googlecode.aviator;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.googlecode.aviator.exception.CompileExpressionErrorException;


public class AviatorEvaluatorUnitTest {
    @Test
    public void testCompileWithoutCache() {
        Expression exp1 = AviatorEvaluator.compile("1+3");
        Expression exp2 = AviatorEvaluator.compile("1+3");
        assertNotNull(exp1);
        assertNotNull(exp2);
        assertNotSame(exp1, exp2);

        assertEquals(4, exp1.execute(null));
        assertEquals(4, exp2.execute(null));
    }


    @Test
    public void testCompileCache() {
        Expression exp1 = AviatorEvaluator.compile("1+3", true);
        Expression exp2 = AviatorEvaluator.compile("1+3", true);
        assertNotNull(exp1);
        assertNotNull(exp2);
        assertSame(exp1, exp2);

        assertEquals(4, exp1.execute(null));
        assertEquals(4, exp2.execute(null));
    }


    @Test
    public void testInvalidateCache() {
        Expression exp1 = AviatorEvaluator.compile("1+3", true);
        Expression exp2 = AviatorEvaluator.compile("1+3", true);
        assertNotNull(exp1);
        assertNotNull(exp2);
        assertSame(exp1, exp2);

        AviatorEvaluator.invalidateCache("1+3");
        Expression exp3 = AviatorEvaluator.compile("1+3", true);
        assertNotSame(exp1, exp3);

        assertEquals(4, exp1.execute(null));
        assertEquals(4, exp2.execute(null));

        assertEquals(4, exp3.execute(null));

    }


    @Test
    public void evaluatorWithoutCache() {
        Map<String, Object> env = new HashMap<String, Object>();
        env.put("a", "hello");
        env.put("b", " world");
        assertEquals("hello world", AviatorEvaluator.execute("a+b", env));
    }


    @Test
    public void evaluatorWithCache() {
        Map<String, Object> env = new HashMap<String, Object>();
        env.put("a", "hello");
        env.put("b", " world");
        assertEquals("hello world", AviatorEvaluator.execute("a+b", env, true));
    }


    @Test(expected = CompileExpressionErrorException.class)
    public void compileBlankExpression1() {
        AviatorEvaluator.compile("");
    }


    @Test(expected = CompileExpressionErrorException.class)
    public void compileBlankExpression2() {
        AviatorEvaluator.compile("    ");
    }


    @Test(expected = CompileExpressionErrorException.class)
    public void executeBlankExpression1() {
        AviatorEvaluator.execute("", null);
    }


    @Test(expected = CompileExpressionErrorException.class)
    public void executeBlankExpression2() {
        AviatorEvaluator.execute("    ");
    }
}
