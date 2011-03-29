package com.googlecode.aviator.runtime.function.system;

import static org.junit.Assert.*;

import org.junit.Test;

import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorObject;


public class RandomFunctionUnitTest {

    @Test
    public void testCall() {
        RandomFunction rand = new RandomFunction();
        AviatorObject result = rand.call(null, new AviatorObject[0]);
        assertTrue(result instanceof AviatorDouble);
        assertFalse(result.getValue(null).equals(rand.call(null, new AviatorObject[0]).getValue(null)));
    }


    @Test(expected = IllegalArgumentException.class)
    public void testCallIllegalArgument() {
        RandomFunction rand = new RandomFunction();
        AviatorObject result = rand.call(null, new AviatorObject[2]);
    }
}
