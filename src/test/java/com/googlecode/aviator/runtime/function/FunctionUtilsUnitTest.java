package com.googlecode.aviator.runtime.function;

import static org.junit.Assert.*;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.seq.SeqMapFunction;
import com.googlecode.aviator.runtime.function.system.BinaryFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;


public class FunctionUtilsUnitTest {

    @Test
    public void testGetFunction_Normal() {

        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorJavaType("map");
        AviatorFunction fun = FunctionUtils.getFunction(0, args, AviatorEvaluator.FUNC_MAP, 2);
        assertNotNull(fun);
        assertTrue(fun instanceof SeqMapFunction);
    }


    @Test
    public void testGetFunction_sub() {
        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorJavaType("-");
        AviatorFunction fun = FunctionUtils.getFunction(0, args, AviatorEvaluator.FUNC_MAP, 2);
        assertNotNull(fun);
        assertTrue(fun instanceof BinaryFunction);
        assertEquals("-sub", fun.getName());
        assertEquals(OperatorType.SUB, ((BinaryFunction) fun).getOpType());

    }


    @Test
    public void testGetFunction_neg() {
        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorJavaType("-");
        AviatorFunction fun = FunctionUtils.getFunction(0, args, AviatorEvaluator.FUNC_MAP, 1);
        assertNotNull(fun);
        assertTrue(fun instanceof BinaryFunction);
        assertEquals("-neg", fun.getName());
        assertEquals(OperatorType.NEG, ((BinaryFunction) fun).getOpType());

    }
}
