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
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import static org.junit.Assert.assertSame;
import java.math.MathContext;
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
  public void testDefaultOptionValues() {
    assertEquals(AviatorEvaluator.getOption(Options.TRACE), false);
    assertEquals(AviatorEvaluator.getOption(Options.TRACE_EVAL), false);
    assertEquals(AviatorEvaluator.getOption(Options.ALWAYS_USE_DOUBLE_AS_DECIMAL), false);
    assertEquals(
        AviatorEvaluator.getOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL), false);
    assertEquals(AviatorEvaluator.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.EVAL);
    assertEquals(AviatorEvaluator.getOption(Options.MATH_CONTEXT), MathContext.DECIMAL128);
  }


  @Test
  public void testSetOptions() {
    try {
      AviatorEvaluator.setOption(Options.TRACE, true);
      assertEquals(AviatorEvaluator.getOption(Options.TRACE), true);
      AviatorEvaluator.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
      assertEquals(AviatorEvaluator.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.COMPILE);
    } finally {
      AviatorEvaluator.setOption(Options.TRACE, false);
      AviatorEvaluator.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.EVAL);
    }
  }


  @Test
  public void testExec() {
    String exp1 = "b-c+a";
    String exp2 = "2*3.14*(R-r)+b/c";
    String exp3 = "f>d?k:a";
    String exp4 = "map.a > map.b ? list[0][1]: y";
    assertEquals(8, AviatorEvaluator.exec(exp1, 6, 2, 4));
    assertEquals(104, AviatorEvaluator.exec(exp1, 99, 3, 8));
    assertEquals(1.14d, AviatorEvaluator.exec(exp1, 3.14, 3, 1));
    assertEquals(6.28, AviatorEvaluator.exec(exp2, 4, 3, 0, 100));
    assertEquals(13.304d, AviatorEvaluator.exec(exp2, 5, 3.2, 4, 2.0));
    assertEquals(3.14d, AviatorEvaluator.exec(exp3, 2, 1, 3.14, 9));
    assertEquals(9, AviatorEvaluator.exec(exp3, 1, 2, 3.14, 9));

    Map<String, Integer> map = new HashMap<String, Integer>();
    map.put("a", 100);
    map.put("b", 99);
    int[][] list = new int[1][2];
    list[0] = new int[2];
    list[0][1] = 2000;
    int y = 999;
    assertEquals(2000L, AviatorEvaluator.exec(exp4, map, list, y));
    map.put("a", 99);
    assertEquals(999L, AviatorEvaluator.exec(exp4, map, list, y));
  }


  @Test(expected = IllegalArgumentException.class)
  public void testExecIllegalArguments() {
    AviatorEvaluator.exec("a-b+c", 1, 2);
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


  @Test(expected = IllegalArgumentException.class)
  public void testSetNullMathContext() {
    AviatorEvaluator.setMathContext(null);
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
