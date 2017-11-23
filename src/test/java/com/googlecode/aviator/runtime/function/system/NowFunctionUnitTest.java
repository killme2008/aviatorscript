package com.googlecode.aviator.runtime.function.system;

import static org.junit.Assert.*;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class NowFunctionUnitTest {

  @Test
  public void testCall() {
    NowFunction now = new NowFunction();
    AviatorLong aviatorLong = (AviatorLong) now.call(null);

    assertEquals(System.currentTimeMillis(), (Long) aviatorLong.getValue(null), 5);
  }


  @Test(expected = IllegalArgumentException.class)
  public void testCall_WithOneArgument() {
    NowFunction now = new NowFunction();
    now.call(null, new AviatorRuntimeJavaType(1));
  }

}
