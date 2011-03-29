package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import java.util.HashSet;

import org.junit.Test;

import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqCountFunctionUnitTest {

    @Test
    public void testCount_Array() {
        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorRuntimeJavaType(new String[10]);
        SeqCountFunction fun = new SeqCountFunction();
        AviatorObject result = fun.call(null, args);
        assertEquals(10, result.getValue(null));
    }


    @Test
    public void testCount_EmptyArray() {
        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorRuntimeJavaType(new String[0]);
        SeqCountFunction fun = new SeqCountFunction();
        AviatorObject result = fun.call(null, args);
        assertEquals(0, result.getValue(null));
    }


    @Test
    public void testCount_Collection() {
        AviatorObject[] args = new AviatorObject[1];
        final HashSet<Integer> set = new HashSet<Integer>();
        set.add(1);
        set.add(2);
        args[0] = new AviatorRuntimeJavaType(set);
        SeqCountFunction fun = new SeqCountFunction();
        AviatorObject result = fun.call(null, args);
        assertEquals(2, result.getValue(null));
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCount_String() {
        AviatorObject[] args = new AviatorObject[1];

        args[0] = new AviatorRuntimeJavaType("hello");
        SeqCountFunction fun = new SeqCountFunction();
        AviatorObject result = fun.call(null, args);
    }

}
