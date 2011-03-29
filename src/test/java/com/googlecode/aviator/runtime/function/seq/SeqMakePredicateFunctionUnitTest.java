package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Test;

import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqMakePredicateFunctionUnitTest {

    @Test
    public void testMakePredicate() {
        SeqMakePredicateFunFunction fun = new SeqMakePredicateFunFunction("eq", OperatorType.EQ);

        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorRuntimeJavaType("hello");

        Map<String, Object> env = new HashMap<String, Object>();
        AviatorJavaType predicateName = (AviatorJavaType) fun.call(env, args);

        assertNotNull(predicateName);
        assertEquals(1, env.size());
        AviatorFunction predicate = (AviatorFunction) env.get(predicateName.getName());
        assertNotNull(predicate);
        AviatorObject result = predicate.call(null, args);
        // equals self
        assertTrue(result.booleanValue(null));

    }


    @Test
    public void testMakePredicate_FixedValue() {
        SeqMakePredicateFunFunction fun =
                new SeqMakePredicateFunFunction("eq", OperatorType.EQ, new AviatorRuntimeJavaType("hello"));
        Map<String, Object> env = new HashMap<String, Object>();
        AviatorJavaType predicateName = (AviatorJavaType) fun.call(env);

        assertNotNull(predicateName);
        assertEquals(1, env.size());
        AviatorFunction predicate = (AviatorFunction) env.get(predicateName.getName());
        assertNotNull(predicate);
        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorRuntimeJavaType("hello");
        AviatorObject result = predicate.call(null, args);
        // equals self
        assertTrue(result.booleanValue(null));

        args[0] = new AviatorRuntimeJavaType("he11o");
        result = predicate.call(null, args);
        // equals self
        assertFalse(result.booleanValue(null));

    }

}
