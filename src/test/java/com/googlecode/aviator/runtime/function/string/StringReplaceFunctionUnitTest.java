package com.googlecode.aviator.runtime.function.string;

import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorString;
import static org.junit.Assert.*;


public class StringReplaceFunctionUnitTest {

  @Test
  public void testReplaceFirst() {
    StringReplaceFirstFunction fun = new StringReplaceFirstFunction();

    assertEquals("string.replace_first", fun.getName());
    Object value =
        fun.call(null, new AviatorString("hello"), new AviatorString("l"), new AviatorString("a"))
            .getValue(null);
    assertEquals("healo", value);
    try {
      fun.call(null, null, new AviatorString("l"), new AviatorString("a")).getValue(null);
      fail();
    } catch (NullPointerException e) {
      // assertEquals("Could not replace with null string",
      // e.getMessage());
    }
  }


  @Test
  public void testReplaceAll() {
    StringReplaceAllFunction fun = new StringReplaceAllFunction();

    assertEquals("string.replace_all", fun.getName());
    Object value =
        fun.call(null, new AviatorString("hello"), new AviatorString("l"), new AviatorString("a"))
            .getValue(null);
    assertEquals("heaao", value);
    try {
      fun.call(null, null, new AviatorString("l"), new AviatorString("a")).getValue(null);
      fail();
    } catch (NullPointerException e) {
      // assertEquals("Could not replace with null string",
      // e.getMessage());
    }
  }

}
