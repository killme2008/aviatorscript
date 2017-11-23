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
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionRuntimeException;


public class AviatorBooleanUnitTest {

  @Test
  public void testAddString() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    AviatorString aviatorString = new AviatorString(" is true");
    assertEquals("true is true", aviatorBoolean.add(aviatorString, null).getValue(null));

  }


  @Test
  public void testAddJavaString() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    AviatorJavaType aviatorString = new AviatorJavaType("s");
    assertEquals("true is true",
        aviatorBoolean.add(aviatorString, createEnvWith("s", " is true")).getValue(null));

  }


  @Test
  public void testAddJavaChar() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    AviatorJavaType aviatorString = new AviatorJavaType("c");
    assertEquals("trueg",
        aviatorBoolean.add(aviatorString, createEnvWith("c", 'g')).getValue(null));

  }


  private Map<String, Object> createEnvWith(String name, Object obj) {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put(name, obj);
    return env;
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddNumber() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    aviatorBoolean.add(AviatorNumber.valueOf(1), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testAddJavaType() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    aviatorBoolean.add(new AviatorJavaType("a"), null);
  }


  @Test
  public void testCompareBoolean() {
    AviatorBoolean aviatorBoolean1 = AviatorBoolean.valueOf(Boolean.TRUE);
    AviatorBoolean aviatorBoolean2 = AviatorBoolean.valueOf(Boolean.TRUE);
    AviatorBoolean aviatorBoolean3 = AviatorBoolean.valueOf(Boolean.FALSE);
    AviatorBoolean aviatorBoolean4 = AviatorBoolean.valueOf(Boolean.FALSE);

    assertEquals(0, aviatorBoolean1.compare(aviatorBoolean2, null));
    assertEquals(1, aviatorBoolean1.compare(aviatorBoolean3, null));
    assertEquals(-1, aviatorBoolean3.compare(aviatorBoolean2, null));
    assertEquals(0, aviatorBoolean3.compare(aviatorBoolean4, null));

  }


  @Test
  public void testNot() {
    assertEquals(Boolean.TRUE, AviatorBoolean.FALSE.not(null).getValue(null));
    assertEquals(Boolean.FALSE, AviatorBoolean.TRUE.not(null).getValue(null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testNeg() {
    AviatorBoolean.TRUE.neg(null);
  }


  @Test
  public void testCompareJavaBoolean() {
    AviatorBoolean t = AviatorBoolean.valueOf(Boolean.TRUE);
    AviatorBoolean f = AviatorBoolean.valueOf(Boolean.FALSE);

    AviatorJavaType javaType = new AviatorJavaType("true");
    assertEquals(0, t.compare(javaType, createEnvWith("true", Boolean.TRUE)));
    assertEquals(-1, f.compare(javaType, createEnvWith("true", Boolean.TRUE)));
    // compre to null value
    assertEquals(1, t.compare(new AviatorJavaType("a"), null));
    assertEquals(1, f.compare(new AviatorJavaType("a"), null));
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareNumber() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    aviatorBoolean.compare(AviatorNumber.valueOf(1), null);
  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testCompareJavaType() {
    AviatorBoolean aviatorBoolean = AviatorBoolean.valueOf(Boolean.TRUE);
    aviatorBoolean.compare(new AviatorJavaType("a"), createEnvWith("a", 4.6));
  }
}
