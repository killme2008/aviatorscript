package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import java.util.LinkedHashSet;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqReduceFunctionUnitTest {

    @Test
    public void testReduce_Array() {
        Integer[] a = new Integer[10];
        for (int i = 0; i < a.length; i++) {
            a[i] = i;
        }

        AviatorObject[] args = new AviatorObject[3];
        args[0] = new AviatorRuntimeJavaType(a);
        args[1] = new AviatorJavaType("+");
        args[2] = new AviatorRuntimeJavaType(0);

        SeqReduceFunction fun = new SeqReduceFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
        assertNotNull(result);
        assertEquals(45, result.getValue(null));

    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testReduce_NullArray() {
        Integer[] a = new Integer[10];
        for (int i = 0; i < a.length; i++) {
            if (i % 2 == 0) {
                a[i] = i;
            }
        }

        AviatorObject[] args = new AviatorObject[3];
        args[0] = new AviatorRuntimeJavaType(a);
        args[1] = new AviatorJavaType("+");
        args[2] = new AviatorRuntimeJavaType(0);

        SeqReduceFunction fun = new SeqReduceFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);

    }


    @Test
    public void testReduce_Collection() {
        LinkedHashSet<Integer> a = new LinkedHashSet<Integer>();
        for (int i = 0; i < 10; i++) {
            a.add(i);
        }

        AviatorObject[] args = new AviatorObject[3];
        args[0] = new AviatorRuntimeJavaType(a);
        args[1] = new AviatorJavaType("+");
        args[2] = new AviatorRuntimeJavaType(0);

        SeqReduceFunction fun = new SeqReduceFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
        assertNotNull(result);
        assertEquals(45, result.getValue(null));

    }


    @Test(expected = IllegalArgumentException.class)
    public void testReduce_IllegalArguments() {
        LinkedHashSet<Integer> a = new LinkedHashSet<Integer>();
        for (int i = 0; i < 10; i++) {
            a.add(i);
        }

        AviatorObject[] args = new AviatorObject[2];
        args[0] = new AviatorRuntimeJavaType(a);
        args[1] = new AviatorJavaType("+");

        SeqReduceFunction fun = new SeqReduceFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
    }


    @Test(expected = IllegalArgumentException.class)
    public void testReduce_String() {

        AviatorObject[] args = new AviatorObject[3];
        args[0] = new AviatorRuntimeJavaType("hello");
        args[1] = new AviatorJavaType("+");
        args[2] = new AviatorRuntimeJavaType(0);

        SeqReduceFunction fun = new SeqReduceFunction();
        AviatorObject result = fun.call(AviatorEvaluator.FUNC_MAP, args);
    }

}
