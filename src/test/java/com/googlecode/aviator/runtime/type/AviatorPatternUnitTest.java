/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.type;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import junit.framework.Assert;


public class AviatorPatternUnitTest {
  @Test
  public void testComparePattern() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    AviatorPattern p2 = new AviatorPattern("[a-zA-Z]+");
    assertEquals(0, p1.compare(p2, null));
    assertEquals(0, p2.compare(p1, null));

    AviatorPattern p3 = new AviatorPattern("[b-cW]+");
    assertTrue(p1.compare(p3, null) < 0);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareBoolean() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.compare(AviatorBoolean.TRUE, null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareString() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.compare(new AviatorString("hello"), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareNumber() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.compare(AviatorNumber.valueOf(400), null);
  }


  @Test
  public void testAddString() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    assertEquals("[a-zA-Z]+ is a pattern",
        p1.add(new AviatorString(" is a pattern"), null).getValue(null));
  }


  @Test
  public void testAddJavaString() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    assertEquals("[a-zA-Z]+ is a pattern",
        p1.add(new AviatorJavaType("s"), this.createEnvWith("s", " is a pattern")).getValue(null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddBoolean() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.add(AviatorBoolean.TRUE, null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddNumber() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.add(AviatorNumber.valueOf(3L), null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddPattern() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.add(new AviatorPattern("\\d+"), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddJavaNumber() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.add(new AviatorJavaType("a"), this.createEnvWith("a", 400.01f));
  }


  private Map<String, Object> createEnvWith(String name, Object obj) {
    Map<String, Object> env = new HashMap<String, Object>();
    if (name != null) {
      env.put(name, obj);
    }
    env.put("true", Boolean.TRUE);
    env.put("false", Boolean.FALSE);
    return env;
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddJavaDate() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.add(new AviatorJavaType("date"), this.createEnvWith("date", new Date()));
  }


  @Test
  public void testMatchString() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    assertTrue((Boolean) p1.match(new AviatorString("hello"), null).getValue(null));
    assertFalse((Boolean) p1.match(new AviatorString("hello world"), null).getValue(null));
  }


  @Test
  public void testMatchJavaString() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    assertTrue((Boolean) p1.match(new AviatorJavaType("s"), this.createEnvWith("s", "hello"))
        .getValue(null));
    assertFalse((Boolean) p1.match(new AviatorJavaType("s"), this.createEnvWith("s", "hello world"))
        .getValue(null));
  }


  @Test
  public void testPatternGroup() {
    //
    AviatorPattern p1 = new AviatorPattern("-\\d+\\.\\d+");
    Map<String, Object> env = new HashMap<String, Object>();
    p1.match(new AviatorString("-3.4"), env);
    assertEquals(1, env.size());
    assertEquals("-3.4", env.get("$0"));

    p1 = new AviatorPattern("^(-?\\d+)(\\.\\d+)?$");
    env.clear();
    p1.match(new AviatorString("-3.4"), env);
    assertEquals(3, env.size());
    assertEquals("-3.4", env.get("$0"));
    assertEquals("-3", env.get("$1"));
    assertEquals(".4", env.get("$2"));

    // Disable putting capturing groups into env
    try {
      AviatorEvaluator.setOption(Options.PUT_CAPTURING_GROUPS_INTO_ENV, false);
      env.clear();
      assertTrue(p1.match(new AviatorString("-3.4"), env).booleanValue(env));
      assertEquals(0, env.size());
    } finally {
      AviatorEvaluator.setOption(Options.PUT_CAPTURING_GROUPS_INTO_ENV, true);
    }
  }


  @Test
  public void testMatchJavaChar() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    assertTrue((Boolean) p1.match(new AviatorJavaType("ch"), this.createEnvWith("ch", 'a'))
        .getValue(null));
    assertFalse((Boolean) p1.match(new AviatorJavaType("ch"), this.createEnvWith("ch", ' '))
        .getValue(null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testMatchBoolean() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.match(AviatorBoolean.TRUE, null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testMatchNumber() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.match(AviatorNumber.valueOf(3.3), null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testMatchPattern() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.match(new AviatorPattern("\\d+"), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testMatchJavaNumber() {
    AviatorPattern p1 = new AviatorPattern("[a-zA-Z]+");
    p1.match(new AviatorJavaType("num"), this.createEnvWith("num", 3000L));

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testNot() {
    new AviatorPattern("\\d+").not(null);

  }


  @Test
  public void testCompareJavaType() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", 1);
    env.put("s", "hello");
    try {
      new AviatorPattern("\\d+").compare(new AviatorJavaType("a"), env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    try {
      new AviatorPattern("\\d+").compare(new AviatorJavaType("s"), env);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }

    assertEquals(1, new AviatorPattern("\\d+").compare(new AviatorJavaType("unknow"), env));

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testNeg() {
    new AviatorPattern("\\d+").neg(null);

  }
}
