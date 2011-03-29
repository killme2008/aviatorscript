package com.googlecode.aviator.runtime.function.math;

import static org.junit.Assert.*;

import java.util.HashMap;
import java.util.Map;

import org.junit.Before;
import org.junit.Test;

import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorString;


public class MathPowFunctionUnitTest {
    AviatorFunction function;


    @Before
    public void setUp() {
        this.function = new MathPowFunction();
    }


    @Test
    public void testCall() {
        assertEquals(Math.pow(3, 2), this.function.call(null, AviatorNumber.valueOf(3), AviatorNumber.valueOf(2))
            .getValue(null));
        assertEquals(Math.pow(10.9, 4.0), this.function.call(null, AviatorNumber.valueOf(10.9),
            AviatorNumber.valueOf(4.0)).getValue(null));
        assertEquals(Math.pow(20, -3), this.function.call(null, AviatorNumber.valueOf(20), AviatorNumber.valueOf(-3))
            .getValue(null));

        Map<String, Object> env = new HashMap<String, Object>();
        env.put("a", 3.0);
        env.put("b", 9.0);

        assertEquals(Math.pow(3.0, 9.0), this.function.call(env, new AviatorJavaType("a"), new AviatorJavaType("b"))
            .getValue(null));
        assertEquals(Math.pow(9.0, 4), this.function.call(env, new AviatorJavaType("b"), AviatorNumber.valueOf(4))
            .getValue(null));
        assertEquals(Math.pow(-2.3, 3.0), this.function
            .call(env, AviatorNumber.valueOf(-2.3), new AviatorJavaType("a")).getValue(null));
    }


    @Test(expected = IllegalArgumentException.class)
    public void testZeroArgument() {
        function.call(null);
    }


    @Test(expected = IllegalArgumentException.class)
    public void test_OneArugments() {
        function.call(null, AviatorNumber.valueOf(3.2));
    }


    @Test(expected = NullPointerException.class)
    public void testNullPointer() {
        function.call(null, AviatorNil.NIL,AviatorNil.NIL);
    }


    @Test(expected = ClassCastException.class)
    public void testClassCastError1() {
        function.call(null, AviatorBoolean.TRUE, AviatorNumber.valueOf(3.2));
    }


    @Test(expected = ClassCastException.class)
    public void testClassCastError2() {
        function.call(null, AviatorNumber.valueOf(3.2), new AviatorString("hello"));
    }
}
