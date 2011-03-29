package com.googlecode.aviator.runtime.method;

import static org.junit.Assert.*;

import java.util.ArrayList;
import java.util.Date;
import java.util.Map;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


public class AviatorMethodUnitTest {
    @Test
    public void testInvoke1() {
        AviatorMethod aviatorMethod = new AviatorMethod("sysdate");
        AviatorObject result = aviatorMethod.invoke(AviatorEvaluator.FUNC_MAP, new ArrayList<AviatorObject>());
        assertNotNull(result);
        assertTrue(result.getValue(null) instanceof Date);
    }


    @Test
    public void testInvoke2() {
        AviatorMethod aviatorMethod = new AviatorMethod("string.contains");
        final ArrayList<AviatorObject> argList = new ArrayList<AviatorObject>();
        argList.add(new AviatorString("hello"));
        argList.add(new AviatorString("hel"));
        AviatorObject result = aviatorMethod.invoke(AviatorEvaluator.FUNC_MAP, argList);
        assertEquals(AviatorBoolean.TRUE, result);
    }

    private static class MockFunction implements AviatorFunction {

        public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
            return null;
        }


        public String getName() {
            return "mock_test";
        }

    }


    @Test(expected = ExpressionRuntimeException.class)
    public void testCouldNotFindMethod() {
        AviatorMethod aviatorMethod = new AviatorMethod("string.contains_not");
        final ArrayList<AviatorObject> argList = new ArrayList<AviatorObject>();
        argList.add(new AviatorString("hello"));
        argList.add(new AviatorString("hel"));
        AviatorObject result = aviatorMethod.invoke(AviatorEvaluator.FUNC_MAP, argList);
        assertEquals(AviatorBoolean.TRUE, result);
    }


    @Test
    public void testInvoke_ReturnNull() {
        AviatorMethod aviatorMethod = new AviatorMethod("mock_test");
        AviatorEvaluator.addFunction(new MockFunction());
        AviatorObject result = aviatorMethod.invoke(AviatorEvaluator.FUNC_MAP, new ArrayList<AviatorObject>());
        assertNotNull(result);
        assertEquals(AviatorNil.NIL, result);
        AviatorEvaluator.removeFunction("mock_test");
    }

}
