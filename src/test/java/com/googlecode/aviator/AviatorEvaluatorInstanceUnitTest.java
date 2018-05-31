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
import java.math.MathContext;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;


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
  public void testInstanceCustomFunctions() {
    this.instance.defineFunction("add", "lambda(x,y) -> println(__env__);x + y end");
    assertEquals(7, this.instance.execute("add(3,4)"));
    try {
      AviatorEvaluator.newInstance().exec("add(3,4)");
      fail();
    } catch (CompileExpressionErrorException e) {
      assertTrue(true);
    }
    assertEquals(17, this.instance.execute("add(8,9)"));
    assertEquals(17, this.instance.exec("add(x,9)", 8));
    try {
      AviatorEvaluator.newInstance().exec("add(1,y)", 3);
      fail();
    } catch (CompileExpressionErrorException e) {
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
    Expression exp1 = this.instance.compile("1+3", true);
    Expression exp2 = this.instance.compile("1+3", true);
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

    this.instance.invalidateCache("1+3");
    Expression exp3 = this.instance.compile("1+3", true);
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
    assertEquals("hello world", this.instance.execute("a+b", env));
  }


  @Test
  public void evaluatorWithCache() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", "hello");
    env.put("b", " world");
    assertEquals("hello world", this.instance.execute("a+b", env, true));
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
