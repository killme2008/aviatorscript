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

import java.util.Map;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * Aviator nil object
 * 
 * @author dennis
 * 
 */
public class AviatorNil extends AviatorObject {
  public static final AviatorNil NIL = new AviatorNil();


  private AviatorNil() {

  }


  @Override
  public AviatorObject add(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case String:
        return new AviatorString("null" + other.getValue(env));
      case JavaType:
        final Object otherValue = other.getValue(env);
        if (TypeUtils.isString(otherValue)) {
          return new AviatorString("null" + otherValue);
        } else {
          return super.add(other, env);
        }
      default:
        return super.add(other, env);
    }
  }


  @Override
  public int compare(AviatorObject other, Map<String, Object> env) {
    if (this == other)
      return 0;
    switch (other.getAviatorType()) {
      case Nil:
        return 0;
      case JavaType:
        if (other.getValue(env) == null) {
          return 0;
        }
    }
    // Any object is greater than nil except nil
    return -1;
  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Nil;
  }


  @Override
  public Object getValue(Map<String, Object> env) {
    return null;
  }

}
