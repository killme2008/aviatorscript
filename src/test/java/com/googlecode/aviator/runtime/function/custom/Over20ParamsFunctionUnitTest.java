package com.googlecode.aviator.runtime.function.custom;

import static org.junit.Assert.*;

import java.util.HashMap;

import org.junit.Test;

import com.googlecode.aviator.AviatorEvaluator;


public class Over20ParamsFunctionUnitTest {
    @Test
    public void testGetFirstNonNullFunctionWith21Params() {
        AviatorEvaluator.addFunction(new GetFirstNonNullFunction());
        HashMap<String, Object> env = new HashMap<String, Object>();

        StringBuilder sb = new StringBuilder("getFirstNonNull(");
        boolean wasFirst = true;
        for (int i = 0; i < 20; i++) {
            env.put("i" + i, null);
            if (wasFirst) {
                sb.append("i" + i);
                wasFirst = false;
            }
            else {
                sb.append(",i" + i);
            }
        }
        Object last = new Object();
        env.put("last", last);
        sb.append(",last)");
        assertSame(last, AviatorEvaluator.execute(sb.toString(), env));
    }


    @Test
    public void testGetFirstNonNullFunctionWith101Params() {
        AviatorEvaluator.addFunction(new GetFirstNonNullFunction());
        HashMap<String, Object> env = new HashMap<String, Object>();

        StringBuilder sb = new StringBuilder("getFirstNonNull(");
        boolean wasFirst = true;
        for (int i = 0; i < 100; i++) {
            env.put("i" + i, null);
            if (wasFirst) {
                sb.append("i" + i);
                wasFirst = false;
            }
            else {
                sb.append(",i" + i);
            }
        }
        Object last = new Object();
        env.put("last", last);
        sb.append(",last)");
        assertSame(last, AviatorEvaluator.execute(sb.toString(), env));
    }


    @Test
    public void testGetFirstNonNullFunctionNestWithManyParams() {
        AviatorEvaluator.addFunction(new GetFirstNonNullFunction());
        HashMap<String, Object> env = new HashMap<String, Object>();

        StringBuilder sb = new StringBuilder("getFirstNonNull(");
        boolean wasFirst = true;
        for (int i = 0; i < 30; i++) {
            env.put("i" + i, null);
            if (wasFirst) {
                sb.append("i" + i);
                wasFirst = false;
            }
            else {
                sb.append(",i" + i);
            }
        }

        sb.append(", getFirstNonNull(");
        wasFirst = true;
        for (int i = 0; i < 30; i++) {
            if (wasFirst) {
                sb.append("i" + i);
                wasFirst = false;
            }
            else {
                sb.append(",i" + i);
            }
        }
        Object last = new Object();
        env.put("last", last);
        sb.append(",last))");

        System.out.println(sb.toString());

        assertSame(last, AviatorEvaluator.execute(sb.toString(), env));
    }

}
