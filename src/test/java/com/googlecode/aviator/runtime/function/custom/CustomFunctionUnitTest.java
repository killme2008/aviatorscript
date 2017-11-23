package com.googlecode.aviator.runtime.function.custom;

import static org.junit.Assert.*;
import java.util.HashMap;
import org.junit.BeforeClass;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;


public class CustomFunctionUnitTest {

  @BeforeClass
  public static void setup() {
    AviatorEvaluator.addFunction(new GetFirstNonNullFunction());
  }


  @Test
  public void testGetFirstNonNullFunctionWith20Params() {
    testGetFirstNonNull(19);
  }


  private void testGetFirstNonNull(int n) {

    HashMap<String, Object> env = new HashMap<String, Object>();
    StringBuilder sb = new StringBuilder("getFirstNonNull(");
    boolean wasFirst = true;
    for (int i = 0; i < n; i++) {
      env.put("i" + i, null);
      if (wasFirst) {
        sb.append("i" + i);
        wasFirst = false;
      } else {
        sb.append(",i" + i);
      }
    }
    Object last = new Object();
    env.put("last", last);
    sb.append(",last)");
    assertSame(last, AviatorEvaluator.execute(sb.toString(), env));
  }


  @Test
  public void testGetFirstNonNullFunctionWith21Params() {
    testGetFirstNonNull(20);
  }


  @Test
  public void testGetFirstNonNullFunctionWith101Params() {
    testGetFirstNonNull(100);
  }


  @Test
  public void testGetFirstNonNullFunctionNestWithManyParams() {
    HashMap<String, Object> env = new HashMap<String, Object>();

    StringBuilder sb = new StringBuilder("getFirstNonNull(");
    boolean wasFirst = true;
    for (int i = 0; i < 30; i++) {
      env.put("i" + i, null);
      if (wasFirst) {
        sb.append("i" + i);
        wasFirst = false;
      } else {
        sb.append(",i" + i);
      }
    }

    sb.append(", getFirstNonNull(");
    wasFirst = true;
    for (int i = 0; i < 30; i++) {
      if (wasFirst) {
        sb.append("i" + i);
        wasFirst = false;
      } else {
        sb.append(",i" + i);
      }
    }
    Object last = new Object();
    env.put("last", last);
    sb.append(",last))");

    System.out.println(sb.toString());

    assertSame(last, AviatorEvaluator.execute(sb.toString(), env));
  }


  @Test
  public void testMyAddFunction() {
    assertEquals(10, AviatorEvaluator.execute("myadd(3,7)"));
    assertEquals(10, AviatorEvaluator.exec("myadd(a,b)", 6, 4));
  }

}
