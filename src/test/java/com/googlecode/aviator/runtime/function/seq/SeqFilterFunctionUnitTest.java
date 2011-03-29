package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;


public class SeqFilterFunctionUnitTest {
    @Test
    public void testFilter_Array() {
        AviatorObject[] args = new AviatorObject[2];
        final String[] strs = new String[10];
        for (int i = 0; i < strs.length; i++) {
            strs[i] = "hello" + i;
        }
        args[0] = new AviatorRuntimeJavaType(strs);
        SeqPredicateFunction predicate =
                new SeqPredicateFunction("eq_temp_1", OperatorType.EQ, new AviatorString("hello1"));
        Map<String, Object> env = new HashMap<String, Object>();
        env.putAll(AviatorEvaluator.FUNC_MAP);
        env.put("eq_temp_1", predicate);
        args[1] = new AviatorJavaType("eq_temp_1");
        SeqFilterFunction fun = new SeqFilterFunction();
        AviatorObject result = fun.call(env, args);
        Object[] array = (Object[]) result.getValue(null);
        assertEquals(1, array.length);
        for (Object i : array) {
            assertEquals("hello1", i);
        }
    }


    @Test
    public void testFilter_Collection() {
        AviatorObject[] args = new AviatorObject[2];
        final List<String> strs = new LinkedList<String>();
        for (int i = 0; i < 10; i++) {
            strs.add("hello" + i);
        }
        args[0] = new AviatorRuntimeJavaType(strs);
        SeqPredicateFunction predicate =
                new SeqPredicateFunction("eq_temp_1", OperatorType.EQ, new AviatorString("hello1"));
        Map<String, Object> env = new HashMap<String, Object>();
        env.putAll(AviatorEvaluator.FUNC_MAP);
        env.put("eq_temp_1", predicate);
        args[1] = new AviatorJavaType("eq_temp_1");
        SeqFilterFunction fun = new SeqFilterFunction();
        AviatorObject result = fun.call(env, args);
        LinkedList list = (LinkedList) result.getValue(null);
        assertEquals(1, list.size());
        for (Object i : list) {
            assertEquals("hello1", i);
        }
    }


    @Test(expected = IllegalArgumentException.class)
    public void testFilter_String() {
        AviatorObject[] args = new AviatorObject[2];

        args[0] = new AviatorRuntimeJavaType("hello");
        args[1] = new AviatorJavaType("string.length");
        SeqFilterFunction fun = new SeqFilterFunction();

        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
    }
}
