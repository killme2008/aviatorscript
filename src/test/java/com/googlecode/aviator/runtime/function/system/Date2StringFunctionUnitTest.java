package com.googlecode.aviator.runtime.function.system;

import java.util.Date;
import org.junit.Test;
import static org.junit.Assert.*;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;


public class Date2StringFunctionUnitTest {

  private Date2StringFunction function = new Date2StringFunction();


  @Test
  public void testCall() {
    assertEquals("date_to_string", function.getName());
    Date date = new Date();
    assertEquals(DateFormatCache.getOrCreateDateFormat("yyyyMMdd").format(date),
        function.call(null, new AviatorRuntimeJavaType(date), new AviatorString("yyyyMMdd"))
            .getValue(null));

    assertEquals(DateFormatCache.getOrCreateDateFormat("yyyy--MM--dd").format(date),
        function.call(null, new AviatorRuntimeJavaType(date), new AviatorString("yyyy--MM--dd"))
            .getValue(null));

  }


  @Test(expected = ClassCastException.class)
  public void testCall_NotDate() {
    assertEquals("date_to_string", function.getName());
    function.call(null, new AviatorRuntimeJavaType(1), new AviatorString("yyyyMMdd"));
  }
}
