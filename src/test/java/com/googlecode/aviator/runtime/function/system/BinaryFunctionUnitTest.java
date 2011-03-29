package com.googlecode.aviator.runtime.function.system;

import static org.junit.Assert.*;

import org.junit.Test;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


public class BinaryFunctionUnitTest {

    @Test
    public void testAddFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.ADD);
        AviatorObject[] args = new AviatorObject[2];
        args[0] = AviatorLong.valueOf(10L);
        args[1] = AviatorLong.valueOf(11L);
        AviatorObject result = fun.call(null, args);
        assertEquals(21L, (Long) result.getValue(null), 0L);

    }


    @Test
    public void testSubFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.SUB);
        AviatorObject[] args = new AviatorObject[2];
        args[0] = AviatorLong.valueOf(10L);
        args[1] = AviatorLong.valueOf(11L);
        AviatorObject result = fun.call(null, args);
        assertEquals(-1L, (Long) result.getValue(null), 0L);

    }


    @Test
    public void testMultFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.MULT);
        AviatorObject[] args = new AviatorObject[2];
        args[0] = AviatorLong.valueOf(10L);
        args[1] = AviatorLong.valueOf(11L);
        AviatorObject result = fun.call(null, args);
        assertEquals(110L, (Long) result.getValue(null), 0L);

    }


    @Test
    public void testDivFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.DIV);
        AviatorObject[] args = new AviatorObject[2];
        args[0] = AviatorLong.valueOf(10L);
        args[1] = AviatorLong.valueOf(11L);
        AviatorObject result = fun.call(null, args);
        assertEquals(0, (Long) result.getValue(null), 0.00);

    }


    @Test
    public void testModFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.MOD);
        AviatorObject[] args = new AviatorObject[2];
        args[0] = AviatorLong.valueOf(10L);
        args[1] = AviatorLong.valueOf(11L);
        AviatorObject result = fun.call(null, args);
        assertEquals(10L, (Long) result.getValue(null), 0L);

    }


    @Test
    public void testNegFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.NEG);
        AviatorObject[] args = new AviatorObject[1];
        args[0] = AviatorLong.valueOf(10L);
        AviatorObject result = fun.call(null, args);
        assertEquals(-10L, (Long) result.getValue(null), 0L);

    }


    @Test
    public void testNotFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.NOT);
        AviatorObject[] args = new AviatorObject[1];
        args[0] = AviatorBoolean.FALSE;
        AviatorObject result = fun.call(null, args);
        assertTrue((Boolean) result.getValue(null));

    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testIllegalFunction() {
        BinaryFunction fun = new BinaryFunction(OperatorType.EQ);
        AviatorObject[] args = new AviatorObject[2];
        args[0] = AviatorLong.valueOf(10L);
        args[1] = AviatorLong.valueOf(11L);
        AviatorObject result = fun.call(null, args);
    }
}
