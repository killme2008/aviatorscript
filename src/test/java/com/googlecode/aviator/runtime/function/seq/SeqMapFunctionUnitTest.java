package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import java.util.LinkedList;
import java.util.List;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqMapFunctionUnitTest {

    @Test
    public void testMap_Array() {
        AviatorObject[] args = new AviatorObject[2];
        final String[] strs = new String[10];
        for (int i = 0; i < strs.length; i++) {
            strs[i] = "hello";
        }
        args[0] = new AviatorRuntimeJavaType(strs);
        args[1] = new AviatorJavaType("string.length");
        SeqMapFunction fun = new SeqMapFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
        Object[] array = (Object[]) result.getValue(null);
        for (Object i : array) {
            assertEquals(5, i);
        }
    }


    @Test
    public void testCount_Collection() {
        AviatorObject[] args = new AviatorObject[2];
        final List<String> strs = new LinkedList<String>();
        for (int i = 0; i < 10; i++) {
            strs.add("hello");
        }
        args[0] = new AviatorRuntimeJavaType(strs);
        args[1] = new AviatorJavaType("string.length");
        SeqMapFunction fun = new SeqMapFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
        LinkedList array = (LinkedList) result.getValue(null);
        for (Object i : array) {
            assertEquals(5, i);
        }
    }


    @Test(expected = IllegalArgumentException.class)
    public void testMap_String() {
        AviatorObject[] args = new AviatorObject[2];

        args[0] = new AviatorRuntimeJavaType("hello");
        args[1] = new AviatorJavaType("string.length");
        SeqMapFunction fun = new SeqMapFunction();

        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
    }

}
