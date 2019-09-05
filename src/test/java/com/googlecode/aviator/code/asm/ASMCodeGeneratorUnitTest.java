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

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.lang.reflect.InvocationTargetException;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.regex.Pattern;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.code.LambdaGenerator;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.PatternToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.parser.ScopeInfo;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;


public class ASMCodeGeneratorUnitTest {
  private ASMCodeGenerator codeGenerator;


  @Before
  public void setUp() {
    final AviatorClassLoader classloader =
        AccessController.doPrivileged(new PrivilegedAction<AviatorClassLoader>() {
          @Override
          public AviatorClassLoader run() {
            return new AviatorClassLoader(Thread.currentThread().getContextClassLoader());
          }
        });

    this.codeGenerator =
        new ASMCodeGenerator(AviatorEvaluator.newInstance(), classloader, System.out, true);
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
    assertTrue(result instanceof Pattern);
    assertEquals("[a-z_A-Z]+", result.toString());
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
    doArithOpTest(OperatorType.ADD);
  }


  @Test
  public void testOnSub() throws Exception {
    doArithOpTest(OperatorType.SUB);
  }


  @Test
  public void testOnMult() throws Exception {
    doArithOpTest(OperatorType.MULT);
  }


  @Test
  public void testOnDiv() throws Exception {
    doArithOpTest(OperatorType.DIV);
  }


  @Test
  public void testOnMod() throws Exception {
    doArithOpTest(OperatorType.MOD);
  }


  @Test
  public void testOnBitAnd() throws Exception {
    doArithOpTest(OperatorType.BIT_AND);
  }


  @Test
  public void testOnBitOr() throws Exception {
    doArithOpTest(OperatorType.BIT_OR);
  }


  @Test
  public void testOnBitXor() throws Exception {
    doArithOpTest(OperatorType.BIT_XOR);
  }


  @Test
  public void testOnShiftLeft() throws Exception {
    doArithOpTest(OperatorType.SHIFT_LEFT);
  }


  @Test
  public void testOnShiftRight() throws Exception {
    doArithOpTest(OperatorType.SHIFT_RIGHT);
  }


  @Test
  public void testOnUnsignedShiftRight() throws Exception {
    doArithOpTest(OperatorType.U_SHIFT_RIGHT);
  }


  public void doArithOpTest(final OperatorType operatorType) throws Exception {
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
        Object result = eval(env);
        assertEquals(15.5, (Double) result, 0.001);
        break;
      case SUB:
        this.codeGenerator.onSub(null);
        this.codeGenerator.onSub(null);
        result = eval(env);
        assertEquals(9.5, (Double) result, 0.001);
        break;
      case MULT:
        this.codeGenerator.onMult(null);
        this.codeGenerator.onMult(null);
        result = eval(env);
        assertEquals(94.5, (Double) result, 0.001);
        break;
      case DIV:
        this.codeGenerator.onDiv(null);
        this.codeGenerator.onDiv(null);
        result = eval(env);
        assertEquals(10.50, (Double) result, 0.001);
        break;
      case MOD:
        this.codeGenerator.onMod(null);
        this.codeGenerator.onMod(null);
        result = eval(env);
        assertEquals(0.0, (Double) result, 0.001);
        break;
    }
  }


  public void doBitOpTests(final OperatorType operatorType) throws Exception {
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
        Object result = eval(env);
        assertEquals(7 | 3 | 2, result);
        break;
      case BIT_AND:
        this.codeGenerator.onBitAnd(null);
        this.codeGenerator.onBitAnd(null);
        result = eval(env);
        assertEquals(7 & 3 & 2, result);
        break;
      case BIT_XOR:
        this.codeGenerator.onBitXor(null);
        this.codeGenerator.onBitXor(null);
        result = eval(env);
        assertEquals(7 ^ 3 ^ 2, result);
        break;
      case SHIFT_LEFT:
        this.codeGenerator.onShiftLeft(null);
        this.codeGenerator.onShiftLeft(null);
        result = eval(env);
        assertEquals(7 << 3 << 2, result);
        break;
      case SHIFT_RIGHT:
        this.codeGenerator.onShiftRight(null);
        this.codeGenerator.onShiftRight(null);
        result = eval(env);
        assertEquals(7 >> 3 >> 2, result);
        break;

      case U_SHIFT_RIGHT:
        this.codeGenerator.onUnsignedShiftRight(null);
        this.codeGenerator.onUnsignedShiftRight(null);
        result = eval(env);
        assertEquals(7 >>> 3 >>> 2, result);
        break;
    }
  }


  private Object eval(final Map<String, Object> env)
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
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnAnd_True() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onAndLeft(null);
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onAndRight(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnJoin_True() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onJoinLeft(null);
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onJoinRight(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnJoin_False() throws Exception {
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onJoinLeft(null);
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onJoinRight(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnNot_True() throws Exception {
    this.codeGenerator.onConstant(Variable.FALSE);
    this.codeGenerator.onNot(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnNot_False() throws Exception {
    this.codeGenerator.onConstant(Variable.TRUE);
    this.codeGenerator.onNot(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.FALSE, result);
  }


  @Test
  public void testOnNeg_Long() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(3L, "3"));
    this.codeGenerator.onNeg(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(-3L, result);
  }

  @Test
  public void testOnBitNot1() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(-3L, "-3"));
    this.codeGenerator.onBitNot(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(2, result);
  }

  @Test
  public void testOnBitNot2() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(3L, "3"));
    this.codeGenerator.onBitNot(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(-4L, result);
  }



  @Test
  public void testOnNeg_Double() throws Exception {
    this.codeGenerator.onConstant(new NumberToken(-3.3d, "-3.3"));
    this.codeGenerator.onNeg(null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals(3.3, result);
  }


  @Test
  public void testOnEq() throws Exception {
    doLogicOpTest(OperatorType.EQ);
  }


  @Test
  public void testOnNeq() throws Exception {
    doLogicOpTest(OperatorType.NEQ);
  }


  @Test
  public void testOnGt() throws Exception {
    doLogicOpTest(OperatorType.GT);
  }


  @Test
  public void testOnGe() throws Exception {
    doLogicOpTest(OperatorType.GE);
  }


  @Test
  public void testOnLt() throws Exception {
    doLogicOpTest(OperatorType.LT);
  }


  @Test
  public void testOnLe() throws Exception {
    doLogicOpTest(OperatorType.LE);
  }


  public void doLogicOpTest(final OperatorType operatorType) throws Exception {
    NumberToken a = new NumberToken(3L, "3");
    NumberToken b = new NumberToken(3L, "3");
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("c", 9L);
    switch (operatorType) {
      case EQ:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onEq(null);
        Object result = eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
      case NEQ:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onNeq(null);
        result = eval(env);
        assertEquals(Boolean.FALSE, result);
        break;
      case GT:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onGt(null);
        result = eval(env);
        assertEquals(Boolean.FALSE, result);
        break;
      case GE:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onGe(null);
        result = eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
      case LT:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(new Variable("c", 0));
        this.codeGenerator.onLt(null);
        result = eval(env);
        assertEquals(Boolean.TRUE, result);
        break;
      case LE:
        this.codeGenerator.onConstant(a);
        this.codeGenerator.onConstant(b);
        this.codeGenerator.onLe(null);
        result = eval(env);
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
    Object result = eval(new HashMap<String, Object>());
    assertEquals(Boolean.TRUE, result);
  }


  @Test
  public void testOnMethod_withoutArguments() throws Exception {
    this.codeGenerator.onMethodName(new Variable("sysdate", -1));
    this.codeGenerator.onMethodInvoke(null, null);
    Object result = eval(new HashMap<String, Object>());
    assertNotNull(result);
    assertTrue(result instanceof Date);
  }

  @Test
  public void testOnLambdaDefine() throws Exception {
    this.codeGenerator.setParser(new Parser() {

      @Override
      public void setCodeGenerator(final CodeGenerator codeGenerator) {

      }

      @Override
      public void restoreScope(final ScopeInfo info) {

      }

      @Override
      public CodeGenerator getCodeGenerator() {
        return ASMCodeGeneratorUnitTest.this.codeGenerator;
      }

      @Override
      public ScopeInfo enterScope() {
        return null;
      }
    });
    assertNull(this.codeGenerator.getLambdaGenerator());
    this.codeGenerator.onLambdaDefineStart(new Variable("lambda", 0));
    LambdaGenerator lambdaGenerator = this.codeGenerator.getLambdaGenerator();
    assertNotNull(lambdaGenerator);
    this.codeGenerator.onLambdaArgument(new Variable("x", 1));
    this.codeGenerator.onLambdaArgument(new Variable("y", 2));
    this.codeGenerator.onLambdaBodyStart(new Variable(">", 3));
    lambdaGenerator.onConstant(new Variable("x", 4));
    lambdaGenerator.onConstant(new Variable("y", 5));
    lambdaGenerator.onAdd(null);
    this.codeGenerator.onLambdaBodyEnd(new Variable("end", 7));
    HashMap<String, Object> env = new HashMap<String, Object>();
    env.put("x", 2);
    env.put("y", 3);
    Object result = eval(env);
    assertTrue(result instanceof LambdaFunction);
    assertEquals(5, ((LambdaFunction) result)
        .call(env, new AviatorJavaType("x"), new AviatorJavaType("y")).getValue(env));
    assertNull(this.codeGenerator.getLambdaGenerator());
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
    this.codeGenerator.onMethodInvoke(null, null);
    Object result = eval(new HashMap<String, Object>());
    assertEquals("llo", result);
  }
}
