package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;

import java.util.HashSet;
import java.util.Set;

import org.junit.Test;

import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqIncludeFunctionUnitTest {

    @Test
    public void testInclude_Array() {
        Integer[] a = new Integer[3];
        a[0] = 1;
        a[1] = -100;

        AviatorObject[] args = new AviatorObject[2];
        args[0] = new AviatorRuntimeJavaType(a);
        args[1] = new AviatorRuntimeJavaType(-100);

        SeqIncludeFunction fun = new SeqIncludeFunction();
        AviatorObject result = fun.call(null, args);
        assertTrue(result.booleanValue(null));

        // contains null Object
        args[1] = AviatorNil.NIL;
        result = fun.call(null, args);
        assertTrue(result.booleanValue(null));

        // not match
        args[1] = new AviatorRuntimeJavaType(1000);
        result = fun.call(null, args);
        assertFalse(result.booleanValue(null));
    }


    @Test
    public void testInclude_HashSet() {
        Set<Integer> a = new HashSet<Integer>();
        a.add(1);
        a.add(-100);
        a.add(null);

        AviatorObject[] args = new AviatorObject[2];
        args[0] = new AviatorRuntimeJavaType(a);
        args[1] = new AviatorRuntimeJavaType(-100);

        SeqIncludeFunction fun = new SeqIncludeFunction();
        AviatorObject result = fun.call(null, args);
        assertTrue(result.booleanValue(null));

        // contains null Object
        args[1] = new AviatorRuntimeJavaType(null);
        result = fun.call(null, args);
        assertTrue(result.booleanValue(null));

        // not match
        args[1] = new AviatorRuntimeJavaType(1000);
        result = fun.call(null, args);
        assertFalse(result.booleanValue(null));

    }


    @Test(expected = IllegalArgumentException.class)
    public void testInclude_String() {
        AviatorObject[] args = new AviatorObject[2];
        args[0] = new AviatorRuntimeJavaType("hello");
        args[1] = new AviatorRuntimeJavaType("h");

        SeqIncludeFunction fun = new SeqIncludeFunction();
        AviatorObject result = fun.call(null, args);
    }

}
