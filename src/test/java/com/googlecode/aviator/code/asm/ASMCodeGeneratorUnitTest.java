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

        this.codeGenerator = new ASMCodeGenerator(classloader, true);
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


    public void doArithOpTest(OperatorType operatorType) throws Exception {
        NumberToken a = new NumberToken(3L, "3");
        NumberToken b = new NumberToken(3.5d, "3.5");
        Map<String, Object> env = new HashMap<String, Object>();
        env.put("c", 9L);
        codeGenerator.onConstant(new Variable("c", 0));
        codeGenerator.onConstant(a);
        codeGenerator.onConstant(b);
        switch (operatorType) {
        case ADD:
            codeGenerator.onAdd(null);
            codeGenerator.onAdd(null);
            Object result = eval(env);
            assertEquals(15.5, (Double) result, 0.001);
            break;
        case SUB:
            codeGenerator.onSub(null);
            codeGenerator.onSub(null);
            result = eval(env);
            assertEquals(9.5, (Double) result, 0.001);
            break;
        case MULT:
            codeGenerator.onMult(null);
            codeGenerator.onMult(null);
            result = eval(env);
            assertEquals(94.5, (Double) result, 0.001);
            break;
        case DIV:
            codeGenerator.onDiv(null);
            codeGenerator.onDiv(null);
            result = eval(env);
            assertEquals(10.50, (Double) result, 0.001);
            break;
        case MOD:
            codeGenerator.onMod(null);
            codeGenerator.onMod(null);
            result = eval(env);
            assertEquals(0.0, (Double) result, 0.001);
            break;
        }
    }


    private Object eval(Map<String, Object> env) throws NoSuchMethodException, IllegalAccessException,
            InvocationTargetException {
        Expression exp = this.codeGenerator.getResult();
        return exp.execute(env);
    }


    @Test
    public void testOnAnd_False() throws Exception {
        codeGenerator.onConstant(Variable.TRUE);
        codeGenerator.onAndLeft(null);
        codeGenerator.onConstant(Variable.FALSE);
        codeGenerator.onAndRight(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.FALSE, result);
    }


    @Test
    public void testOnAnd_True() throws Exception {
        codeGenerator.onConstant(Variable.TRUE);
        codeGenerator.onAndLeft(null);
        codeGenerator.onConstant(Variable.TRUE);
        codeGenerator.onAndRight(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.TRUE, result);
    }


    @Test
    public void testOnJoin_True() throws Exception {
        codeGenerator.onConstant(Variable.TRUE);
        codeGenerator.onJoinLeft(null);
        codeGenerator.onConstant(Variable.FALSE);
        codeGenerator.onJoinRight(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.TRUE, result);
    }


    @Test
    public void testOnJoin_False() throws Exception {
        codeGenerator.onConstant(Variable.FALSE);
        codeGenerator.onJoinLeft(null);
        codeGenerator.onConstant(Variable.FALSE);
        codeGenerator.onJoinRight(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.FALSE, result);
    }


    @Test
    public void testOnNot_True() throws Exception {
        codeGenerator.onConstant(Variable.FALSE);
        codeGenerator.onNot(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.TRUE, result);
    }


    @Test
    public void testOnNot_False() throws Exception {
        codeGenerator.onConstant(Variable.TRUE);
        codeGenerator.onNot(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.FALSE, result);
    }


    @Test
    public void testOnNeg_Long() throws Exception {
        codeGenerator.onConstant(new NumberToken(3L, "3"));
        codeGenerator.onNeg(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(-3L, result);
    }


    @Test
    public void testOnNeg_Double() throws Exception {
        codeGenerator.onConstant(new NumberToken(-3.3d, "-3.3"));
        codeGenerator.onNeg(null);
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


    public void doLogicOpTest(OperatorType operatorType) throws Exception {
        NumberToken a = new NumberToken(3L, "3");
        NumberToken b = new NumberToken(3L, "3");
        Map<String, Object> env = new HashMap<String, Object>();
        env.put("c", 9L);
        switch (operatorType) {
        case EQ:
            codeGenerator.onConstant(a);
            codeGenerator.onConstant(b);
            codeGenerator.onEq(null);
            Object result = eval(env);
            assertEquals(Boolean.TRUE, result);
            break;
        case NEQ:
            codeGenerator.onConstant(a);
            codeGenerator.onConstant(b);
            codeGenerator.onNeq(null);
            result = eval(env);
            assertEquals(Boolean.FALSE, result);
            break;
        case GT:
            codeGenerator.onConstant(a);
            codeGenerator.onConstant(b);
            codeGenerator.onGt(null);
            result = eval(env);
            assertEquals(Boolean.FALSE, result);
            break;
        case GE:
            codeGenerator.onConstant(a);
            codeGenerator.onConstant(b);
            codeGenerator.onGe(null);
            result = eval(env);
            assertEquals(Boolean.TRUE, result);
            break;
        case LT:
            codeGenerator.onConstant(a);
            codeGenerator.onConstant(new Variable("c", 0));
            codeGenerator.onLt(null);
            result = eval(env);
            assertEquals(Boolean.TRUE, result);
            break;
        case LE:
            codeGenerator.onConstant(a);
            codeGenerator.onConstant(b);
            codeGenerator.onLe(null);
            result = eval(env);
            assertEquals(Boolean.TRUE, result);
            break;
        }

    }


    @Test
    public void testOnMatch() throws Exception {
        codeGenerator.onConstant(new StringToken("killme2008@gmail.com", 0));
        codeGenerator.onConstant(new PatternToken("^[\\w\\-]([\\.\\w])+[\\w]+@([\\w\\-]+\\.)+[a-z]{2,4}$", 1));
        codeGenerator.onMatch(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals(Boolean.TRUE, result);
    }


    @Test
    public void testOnMethod_withoutArguments() throws Exception {
        codeGenerator.onMethodName(new Variable("sysdate", -1));
        codeGenerator.onMethodInvoke(null);
        Object result = eval(new HashMap<String, Object>());
        assertNotNull(result);
        assertTrue(result instanceof Date);
    }


    @Test
    public void testOnMethod_withTwoArguments() throws Exception {
        codeGenerator.onMethodName(new Variable("string.substring", -1));
        codeGenerator.onConstant(new StringToken("hello", -1));
        codeGenerator.onMethodParameter(null);
        codeGenerator.onConstant(new NumberToken(2L, "2"));
        codeGenerator.onMethodParameter(null);
        codeGenerator.onConstant(new NumberToken(5L, "5"));
        codeGenerator.onMethodParameter(null);
        codeGenerator.onMethodInvoke(null);
        Object result = eval(new HashMap<String, Object>());
        assertEquals("llo", result);
    }
}
