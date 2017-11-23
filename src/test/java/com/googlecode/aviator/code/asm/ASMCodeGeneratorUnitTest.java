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
package com.googlecode.aviator.code.asm;

import static org.junit.Assert.*;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;


public class ASMCodeGeneratorUnitTest {
  private ASMCodeGenerator codeGenerator;


  @Before
  public void setUp() {
    final AviatorClassLoader classloader =
        AccessController.doPrivileged(new PrivilegedAction<AviatorClassLoader>() {
          public AviatorClassLoader run() {
            return new AviatorClassLoader(Thread.currentThread().getContextClassLoader());
          }
        });

    this.codeGenerator = new ASMCodeGenerator(classloader, System.out, true);
    this.codeGenerator.start();
  }


  @Test
  public void testOnConstant_Nil() throws Exception {
    this.codeGenerator.onConstant(Variable.NIL);
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertNull(result);
  }


  @Test
  public void testOnConstant_Long() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(3L, "3"));
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertEquals(3L, result);
  }


  @Test
  public void testOnConstant_Double() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(3.3D, "3.3"));
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertEquals(3.3D, result);
  }


  @Test
  public void testOnConstant_Boolean_False() throws Exception {
    this.codeGenerator.onConstant(Variable.FALSE);
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnConstant_Boolean_True() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnConstant_String() throws Exception {
    this.codeGenerator.onConstant(new StringToken("hello", 0));
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertEquals("hello", result);
  }


  @Test
  public void testOnConstant_Pattern() throws Exception {
    this.codeGenerator.onConstant(new PatternToken("[a-z_A-Z]+", 0));
    Expression exp = this.codeGenerator.getResult();
    Object result = exp.execute();
    assertEquals("/[a-z_A-Z]+/", result);
  }


  @Test
  public void testOnConstant_Variable() throws Exception {
    this.codeGenerator.onConstant(new Variable("a", 0));
    Expression exp = this.codeGenerator.getResult();
    HashMap<String, Object> env = new HashMap<String, Object>();
    long now = System.currentTimeMillis();
    env.put("a", now);
    Object result = exp.execute(env);
    assertEquals(now, result);
  }


  @Test
  public void testOnAdd() throws Exception {
    this.doArithOpTest(OperatorType.ADD);
  }


  @Test
  public void testOnSub() throws Exception {
    this.doArithOpTest(OperatorType.SUB);
  }


  @Test
  public void testOnMult() throws Exception {
    this.doArithOpTest(OperatorType.MULT);
  }


  @Test
  public void testOnDiv() throws Exception {
    this.doArithOpTest(OperatorType.DIV);
  }


  @Test
  public void testOnMod() throws Exception {
    this.doArithOpTest(OperatorType.MOD);
  }


  @Test
  public void testOnBitAnd() throws Exception {
    this.doArithOpTest(OperatorType.BIT_AND);
  }


  @Test
  public void testOnBitOr() throws Exception {
    this.doArithOpTest(OperatorType.BIT_OR);
  }


  @Test
  public void testOnBitXor() throws Exception {
    this.doArithOpTest(OperatorType.BIT_XOR);
  }


  @Test
  public void testOnShiftLeft() throws Exception {
    this.doArithOpTest(OperatorType.SHIFT_LEFT);
  }


  @Test
  public void testOnShiftRight() throws Exception {
    this.doArithOpTest(OperatorType.SHIFT_RIGHT);
  }


  @Test
  public void testOnUnsignedShiftRight() throws Exception {
    this.doArithOpTest(OperatorType.U_SHIFT_RIGHT);
  }


  public void doArithOpTest(OperatorType operatorType) throws Exception {
    NumberToken a = new NumberToken(3L, "3");
    NumberToken b = new NumberToken(3.5d, "3.5");
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("c", 9L);
    this.codeGenerator.onConstant(new Variable("c", 0));
    this.codeGenerator.onConstant(a);
    this.codeGenerator.onConstant(b);
    switch (operatorType) {
      case ADD:
        this.codeGenerator.onAdd(null);
        this.codeGenerator.onAdd(null);
        Object result = this.eval(env);
        assertEquals(15.5, (Double) result, 0.001);
        break;
      case SUB:
        this.codeGenerator.onSub(null);
        this.codeGenerator.onSub(null);
        result = this.eval(env);
        assertEquals(9.5, (Double) result, 0.001);
        break;
      case MULT:
        this.codeGenerator.onMult(null);
        this.codeGenerator.onMult(null);
        result = this.eval(env);
        assertEquals(94.5, (Double) result, 0.001);
        break;
      case DIV:
        this.codeGenerator.onDiv(null);
        this.codeGenerator.onDiv(null);
        result = this.eval(env);
        assertEquals(10.50, (Double) result, 0.001);
        break;
      case MOD:
        this.codeGenerator.onMod(null);
        this.codeGenerator.onMod(null);
        result = this.eval(env);
        assertEquals(0.0, (Double) result, 0.001);
        break;
    }
  }


  public void doBitOpTests(OperatorType operatorType) throws Exception {
    NumberToken a = new NumberToken(99L, "3");
    NumberToken b = new NumberToken(2, "2");
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("c", 9L);
    this.codeGenerator.onConstant(new Variable("c", 7));
    this.codeGenerator.onConstant(a);
    this.codeGenerator.onConstant(b);
    switch (operatorType) {
      case BIT_OR:
        this.codeGenerator.onBitOr(null);
        this.codeGenerator.onBitOr(null);
        Object result = this.eval(env);
        assertEquals(7 | 3 | 2, result);
        break;
      case BIT_AND:
        this.codeGenerator.onBitAnd(null);
        this.codeGenerator.onBitAnd(null);
        result = this.eval(env);
        assertEquals(7 & 3 & 2, result);
        break;
      case BIT_XOR:
        this.codeGenerator.onBitXor(null);
        this.codeGenerator.onBitXor(null);
        result = this.eval(env);
        assertEquals(7 ^ 3 ^ 2, result);
        break;
      case SHIFT_LEFT:
        this.codeGenerator.onShiftLeft(null);
        this.codeGenerator.onShiftLeft(null);
        result = this.eval(env);
        assertEquals(7 << 3 << 2, result);
        break;
      case SHIFT_RIGHT:
        this.codeGenerator.onShiftRight(null);
        this.codeGenerator.onShiftRight(null);
        result = this.eval(env);
        assertEquals(7 >> 3 >> 2, result);
        break;

      case U_SHIFT_RIGHT:
        this.codeGenerator.onUnsignedShiftRight(null);
        this.codeGenerator.onUnsignedShiftRight(null);
        result = this.eval(env);
        assertEquals(7 >>> 3 >>> 2, result);
        break;
    }
  }


  private Object eval(Map<String, Object> env)
      throws NoSuchMethodException, IllegalAccessException, InvocationTargetException {
    Expression exp = this.codeGenerator.getResult();
    return exp.execute(env);
  }


  @Test
  public void testOnAnd_False() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onAndLeft(null);
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onAndRight(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnAnd_True() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onAndLeft(null);
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onAndRight(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnJoin_True() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onJoinLeft(null);
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onJoinRight(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnJoin_False() throws Exception {
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onJoinLeft(null);
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onJoinRight(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnNot_True() throws Exception {
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onNot(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnNot_False() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onNot(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnNeg_Long() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(3L, "3"));
    this.codeGenerator.onNeg(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(-3L, result);
  }

  @Test
  public void testOnBitNot1() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(-3L, "-3"));
    this.codeGenerator.onBitNot(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(2, result);
  }

  @Test
  public void testOnBitNot2() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(3L, "3"));
    this.codeGenerator.onBitNot(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(-4L, result);
  }



  @Test
  public void testOnNeg_Double() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(-3.3d, "-3.3"));
    this.codeGenerator.onNeg(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(3.3, result);
  }


  @Test
  public void testOnEq() throws Exception {
    this.doLogicOpTest(OperatorType.EQ);
  }


  @Test
  public void testOnNeq() throws Exception {
    this.doLogicOpTest(OperatorType.NEQ);
  }


  @Test
  public void testOnGt() throws Exception {
    this.doLogicOpTest(OperatorType.GT);
  }


  @Test
  public void testOnGe() throws Exception {
    this.doLogicOpTest(OperatorType.GE);
  }


  @Test
  public void testOnLt() throws Exception {
    this.doLogicOpTest(OperatorType.LT);
  }


  @Test
  public void testOnLe() throws Exception {
    this.doLogicOpTest(OperatorType.LE);
  }


  public void doLogicOpTest(OperatorType operatorType) throws Exception {
    NumberToken a = new NumberToken(3L, "3");
    NumberToken b = new NumberToken(3L, "3");
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("c", 9L);
    switch (operatorType) {
      case EQ:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onEq(null);
        Object result = this.eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
      case NEQ:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onNeq(null);
        result = this.eval(env);
        assertEquals(Boolean.FALSE, result);
        break;
      case GT:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onGt(null);
        result = this.eval(env);
        assertEquals(Boolean.FALSE, result);
        break;
      case GE:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onGe(null);
        result = this.eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
      case LT:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(new Variable("c", 0));
        this.codeGenerator.onLt(null);
        result = this.eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
      case LE:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onLe(null);
        result = this.eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
    }

  }


  @Test
  public void testOnMatch() throws Exception {
    this.codeGenerator.onConstant(new StringToken("killme2008@gmail.com", 0));
    this.codeGenerator
        .onConstant(new PatternToken("^[\\w\\-]([\\.\\w])+[\\w]+@([\\w\\-]+\\.)+[a-z]{2,4}$", 1));
    this.codeGenerator.onMatch(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnMethod_withoutArguments() throws Exception {
    this.codeGenerator.onMethodName(new Variable("sysdate", -1));
    this.codeGenerator.onMethodInvoke(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertNotNull(result);
    assertTrue(result instanceof Date);
  }


  @Test
  public void testOnMethod_withTwoArguments() throws Exception {
    this.codeGenerator.onMethodName(new Variable("string.substring", -1));
    this.codeGenerator.onConstant(new StringToken("hello", -1));
    this.codeGenerator.onMethodParameter(null);
    this.codeGenerator.onConstant(new NumberToken(2L, "2"));
    this.codeGenerator.onMethodParameter(null);
    this.codeGenerator.onConstant(new NumberToken(5L, "5"));
    this.codeGenerator.onMethodParameter(null);
    this.codeGenerator.onMethodInvoke(null);
    Object result = this.eval(new HashMap<String, Object>());
    assertEquals("llo", result);
  }
}
