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

import static org.junit.Assert.*;
import java.util.HashMap;
import java.util.Map;
import junit.framework.Assert;
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionRuntimeException;


public class AviatorNilUnitTest {
  // Any object is greater than nil except nil
  @Test
  public void testCompare() {
    assertEquals(0, AviatorNil.NIL.compare(AviatorNil.NIL, null));

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", null);
    env.put("i", 1);
    env.put("f", 3.14f);
    env.put("d", -100.0);
    env.put("ch", 'a');
    env.put("s", "hello");
    assertEquals(0, AviatorNil.NIL.compare(AviatorNil.NIL, null));
    assertEquals(0, AviatorNil.NIL.compare(new AviatorJavaType("a"), env));
    assertEquals(0, new AviatorJavaType("a").compare(AviatorNil.NIL, env));

    assertEquals(1, AviatorNumber.valueOf(1).compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(AviatorNumber.valueOf(1), env));
    assertEquals(1, AviatorNumber.valueOf(-1000).compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(AviatorNumber.valueOf(-1000), env));
    assertEquals(1, AviatorBoolean.TRUE.compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(AviatorBoolean.TRUE, env));
    assertEquals(1, AviatorBoolean.FALSE.compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(AviatorBoolean.FALSE, env));
    assertEquals(1, new AviatorString("").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorString(""), env));
    assertEquals(1, new AviatorString(" ").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorString(" "), env));
    assertEquals(1, new AviatorString("hello").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorString("hello"), env));
    assertEquals(1, new AviatorString("-100").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorString("-100"), env));
    assertEquals(1, new AviatorPattern("\\d+").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorPattern("\\d+"), env));
    assertEquals(1, new AviatorPattern("[\\w_]+\\.[\\w_]").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorPattern("[\\w_]+\\.[\\w_]"), env));

    assertEquals(1, new AviatorJavaType("i").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorJavaType("i"), env));

    assertEquals(1, new AviatorJavaType("f").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorJavaType("f"), env));

    assertEquals(1, new AviatorJavaType("d").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorJavaType("d"), env));

    assertEquals(1, new AviatorJavaType("ch").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorJavaType("ch"), env));

    assertEquals(1, new AviatorJavaType("s").compare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.compare(new AviatorJavaType("s"), env));
  }


  @Test
  public void testAddString() {
    assertEquals("null", AviatorNil.NIL.add(new AviatorString(""), null).getValue(null));
    assertEquals("null hello",
        AviatorNil.NIL.add(new AviatorString(" hello"), null).getValue(null));

    try {
      AviatorNil.NIL.add(new AviatorJavaType("a"), null);
      Assert.fail();
    } catch (ExpressionRuntimeException e) {

    }
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", " hello");
    assertEquals("null hello", AviatorNil.NIL.add(new AviatorJavaType("a"), env).getValue(env));
  }
}
