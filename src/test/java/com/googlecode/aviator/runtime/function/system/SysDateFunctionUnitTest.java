package com.googlecode.aviator.runtime.function.system;

import static org.junit.Assert.*;
import java.util.Date;
import org.junit.Test;
import com.googlecode.aviator.runtime.function.system.SysDateFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;


public class SysDateFunctionUnitTest {
  @Test
  public void testCall() {
    SysDateFunction fun = new SysDateFunction();

    AviatorObject result = fun.call(null);
    assertNotNull(result);
    assertTrue(result.getValue(null) instanceof Date);
  }


  @Test(expected = IllegalArgumentException.class)
  public void hasArugment() {
    SysDateFunction fun = new SysDateFunction();
    fun.call(null, AviatorBoolean.TRUE);

  }
}
