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
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;


public class PrintFunctionUnitTest {
  private PrintFunction fun;
  PrintStream systemOut;


  @Before
  public void setUp() {
    this.fun = new PrintFunction();
    this.systemOut = System.out;
  }


  @After
  public void tearDown() {
    System.setOut(this.systemOut);
  }


  @Test(expected = IllegalArgumentException.class)
  public void testCall_WithEmpyArguments() throws Exception {
    this.fun.call(null);
  }


  @Test
  public void testCall_WithOneArgument() throws Exception {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    System.setOut(new PrintStream(out));
    this.fun.call(null, new AviatorString("hello"));
    out.flush();
    out.close();
    byte[] data = out.toByteArray();
    assertEquals("hello", new String(data));

  }


  @Test
  public void testCall_WithTwoArgument() throws Exception {
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("out", out);
    this.fun.call(env, new AviatorJavaType("out"), new AviatorString("hello"));
    out.flush();
    out.close();
    byte[] data = out.toByteArray();
    assertEquals("hello", new String(data));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testCall_WithFourArgument() throws Exception {
    this.fun.call(null, new AviatorRuntimeJavaType(0), new AviatorRuntimeJavaType(0),
        new AviatorRuntimeJavaType(0), new AviatorRuntimeJavaType(0));

  }

}
