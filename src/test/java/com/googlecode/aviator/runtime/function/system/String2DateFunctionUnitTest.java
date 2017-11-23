package com.googlecode.aviator.runtime.function.system;

import java.util.Date;
import org.junit.Test;
import static org.junit.Assert.*;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;


public class String2DateFunctionUnitTest {

  private String2DateFunction function = new String2DateFunction();


  @Test
  public void testCall() {
    assertEquals("string_to_date", function.getName());
    String source = "2011-09-17";
    Date date = (Date) function
        .call(null, new AviatorRuntimeJavaType(source), new AviatorString("yyyy-MM-dd"))
        .getValue(null);

    assertNotNull(date);
    assertEquals(111, date.getYear());
    assertEquals(8, date.getMonth());
    assertEquals(17, date.getDate());

  }


  @Test(expected = ClassCastException.class)
  public void testCall_NotDate() {
    assertEquals("string_to_date", function.getName());
    function.call(null, new AviatorRuntimeJavaType(1), new AviatorString("yyyyMMdd"));
  }
}
