package com.googlecode.aviator.runtime.function.system;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.PrintStream;
import java.util.HashMap;
import java.util.Map;

import org.junit.After;
import org.junit.Before;
import org.junit.Test;

import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


public class PrintlnFunctionUnitTest {
    private PrintlnFunction fun;
    PrintStream systemOut;


    @Before
    public void setUp() {
        fun = new PrintlnFunction();
        systemOut = System.out;
    }


    @After
    public void tearDown() {
        System.setOut(systemOut);
    }


    @Test
    public void testCall_WithEmpyArguments() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        fun.call(null, new AviatorObject[0]);
        out.flush();
        out.close();
        String lineSeparator = System.getProperty("line.separator");
        byte[] data = out.toByteArray();
        assertEquals(lineSeparator, new String(data));
    }


    @Test
    public void testCall_WithOneArgument() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        AviatorObject[] args = new AviatorObject[1];
        args[0] = new AviatorString("hello");
        fun.call(null, args);
        out.flush();
        out.close();
        String lineSeparator = System.getProperty("line.separator");
        byte[] data = out.toByteArray();
        assertEquals("hello" + lineSeparator, new String(data));

    }


    @Test
    public void testCall_WithTwoArgument() throws Exception {
        ByteArrayOutputStream out = new ByteArrayOutputStream();
        System.setOut(new PrintStream(out));
        AviatorObject[] args = new AviatorObject[2];
        args[0] = new AviatorJavaType("out");
        args[1] = new AviatorString("hello");
        Map<String, Object> env = new HashMap<String, Object>();
        env.put("out", out);
        fun.call(env, args);
        out.flush();
        out.close();
        String lineSeparator = System.getProperty("line.separator");
        byte[] data = out.toByteArray();
        assertEquals("hello" + lineSeparator, new String(data));

    }


    @Test(expected = IllegalArgumentException.class)
    public void testCall_WithFourArgument() throws Exception {
        fun.call(null, new AviatorObject[4]);

    }

}
