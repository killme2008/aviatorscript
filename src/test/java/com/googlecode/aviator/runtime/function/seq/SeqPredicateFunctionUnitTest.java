package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import org.junit.Test;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqPredicateFunctionUnitTest {

    @Test
    public void testPredicate_eq() {
        SeqPredicateFunction fun = new SeqPredicateFunction("eq", OperatorType.EQ, new AviatorRuntimeJavaType("hello"));

        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
        assertTrue(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("he1lo"));
        assertFalse(result.booleanValue(null));
    }


    @Test
    public void testPredicate_neq() {
        SeqPredicateFunction fun =
                new SeqPredicateFunction("neq", OperatorType.NEQ, new AviatorRuntimeJavaType("hello"));

        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
        assertFalse(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("he1lo"));
        assertTrue(result.booleanValue(null));
    }


    @Test
    public void testPredicate_gt() {
        SeqPredicateFunction fun = new SeqPredicateFunction("gt", OperatorType.GT, new AviatorRuntimeJavaType("hello"));

        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
        assertFalse(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("iello"));
        assertTrue(result.booleanValue(null));
    }


    @Test
    public void testPredicate_ge() {
        SeqPredicateFunction fun = new SeqPredicateFunction("ge", OperatorType.GE, new AviatorRuntimeJavaType("hello"));

        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
        assertTrue(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("iello"));
        assertTrue(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("aello"));
        assertFalse(result.booleanValue(null));
    }


    @Test
    public void testPredicate_lt() {
        SeqPredicateFunction fun = new SeqPredicateFunction("lt", OperatorType.LT, new AviatorRuntimeJavaType("hello"));

        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
        assertFalse(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("ae1lo"));
        assertTrue(result.booleanValue(null));
    }


    @Test
    public void testPredicate_le() {
        SeqPredicateFunction fun = new SeqPredicateFunction("le", OperatorType.LE, new AviatorRuntimeJavaType("hello"));

        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
        assertTrue(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("ae1lo"));
        assertTrue(result.booleanValue(null));

        result = fun.call(null, new AviatorRuntimeJavaType("ie1lo"));
        assertFalse(result.booleanValue(null));

    }


    @Test(expected = IllegalArgumentException.class)
    public void testPredicate_IllegalArguments() {
        SeqPredicateFunction fun = new SeqPredicateFunction("le", OperatorType.LE, new AviatorRuntimeJavaType("hello"));
        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"),null);

    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testPredicate_IllegalOperator() {
        SeqPredicateFunction fun =
                new SeqPredicateFunction("and", OperatorType.AND, new AviatorRuntimeJavaType("hello"));
        AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    }
}
