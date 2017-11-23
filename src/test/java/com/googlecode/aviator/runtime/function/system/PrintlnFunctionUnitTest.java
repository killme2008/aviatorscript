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
import com.googlecode.aviator.runtime.type.AviatorString;


public class PrintlnFunctionUnitTest {
  private PrintlnFunction fun;
  PrintStream systemOut;


  @Before
  public void setUp() {
    this.fun = new PrintlnFunction();
    this.systemOut = System.out;
  }


  @After
  public void tearDown() {
    System.setOut(this.systemOut);
  }


  @Test
  public void testCall_WithEmpyArguments() throws Exception {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    System.setOut(new PrintStream(out));
    this.fun.call(null);
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
    this.fun.call(null, new AviatorString("hello"));
    out.flush();
    out.close();
    String lineSeparator = System.getProperty("line.separator");
    byte[] data = out.toByteArray();
    assertEquals("hello" + lineSeparator, new String(data));

  }


  @Test
  public void testCall_WithTwoArgument() throws Exception {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("out", out);
    this.fun.call(env, new AviatorJavaType("out"), new AviatorString("hello"));
    out.flush();
    out.close();
    String lineSeparator = System.getProperty("line.separator");
    byte[] data = out.toByteArray();
    assertEquals("hello" + lineSeparator, new String(data));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testCall_WithFourArgument() throws Exception {
    this.fun.call(null, new AviatorJavaType("out"), new AviatorString("1"), new AviatorString("1"),
        new AviatorString("1"));

  }

}
