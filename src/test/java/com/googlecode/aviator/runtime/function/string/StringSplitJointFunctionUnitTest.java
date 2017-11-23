package com.googlecode.aviator.runtime.function.string;

import java.util.ArrayList;
import java.util.List;
import org.junit.Test;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;
import static org.junit.Assert.*;


public class StringSplitJointFunctionUnitTest {

  @Test
  public void testSplitJoin() {
    StringSplitFunction splitFn = new StringSplitFunction();
    StringJoinFunction joinFn = new StringJoinFunction();

    assertEquals("string.split", splitFn.getName());
    assertEquals("string.join", joinFn.getName());

    String[] tmps = (String[]) splitFn
        .call(null, new AviatorString("a,b,c,d,e,f,g"), new AviatorString(",")).getValue(null);

    assertNotNull(tmps);
    assertEquals(7, tmps.length);
    assertArrayEquals(new String[] {"a", "b", "c", "d", "e", "f", "g"}, tmps);

    assertEquals("a b c d e f g", (String) joinFn
        .call(null, new AviatorRuntimeJavaType(tmps), new AviatorString(" ")).getValue(null));
    assertEquals("abcdefg",
        (String) joinFn.call(null, new AviatorRuntimeJavaType(tmps)).getValue(null));
  }


  @Test
  public void testJoinWithCollection() {
    StringJoinFunction joinFn = new StringJoinFunction();
    List<String> list = new ArrayList<String>();
    list.add("hello");
    list.add("world");
    list.add("aviator");
    assertEquals("hello world aviator", (String) joinFn
        .call(null, new AviatorRuntimeJavaType(list), new AviatorString(" ")).getValue(null));
  }

}
