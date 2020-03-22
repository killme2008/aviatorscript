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
    assertEquals(0, AviatorNil.NIL.innerCompare(AviatorNil.NIL, null));

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", null);
    env.put("i", 1);
    env.put("f", 3.14f);
    env.put("d", -100.0);
    env.put("ch", 'a');
    env.put("s", "hello");
    assertEquals(0, AviatorNil.NIL.innerCompare(AviatorNil.NIL, null));
    assertEquals(0, AviatorNil.NIL.innerCompare(new AviatorJavaType("a"), env));
    assertEquals(0, new AviatorJavaType("a").innerCompare(AviatorNil.NIL, env));

    assertEquals(1, AviatorNumber.valueOf(1).innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(AviatorNumber.valueOf(1), env));
    assertEquals(1, AviatorNumber.valueOf(-1000).innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(AviatorNumber.valueOf(-1000), env));
    assertEquals(1, AviatorBoolean.TRUE.innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(AviatorBoolean.TRUE, env));
    assertEquals(1, AviatorBoolean.FALSE.innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(AviatorBoolean.FALSE, env));
    assertEquals(1, new AviatorString("").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorString(""), env));
    assertEquals(1, new AviatorString(" ").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorString(" "), env));
    assertEquals(1, new AviatorString("hello").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorString("hello"), env));
    assertEquals(1, new AviatorString("-100").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorString("-100"), env));
    assertEquals(1, new AviatorPattern("\\d+").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorPattern("\\d+"), env));
    assertEquals(1, new AviatorPattern("[\\w_]+\\.[\\w_]").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorPattern("[\\w_]+\\.[\\w_]"), env));

    assertEquals(1, new AviatorJavaType("i").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorJavaType("i"), env));

    assertEquals(1, new AviatorJavaType("f").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorJavaType("f"), env));

    assertEquals(1, new AviatorJavaType("d").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorJavaType("d"), env));

    assertEquals(1, new AviatorJavaType("ch").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorJavaType("ch"), env));

    assertEquals(1, new AviatorJavaType("s").innerCompare(AviatorNil.NIL, env));
    assertEquals(-1, AviatorNil.NIL.innerCompare(new AviatorJavaType("s"), env));
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
