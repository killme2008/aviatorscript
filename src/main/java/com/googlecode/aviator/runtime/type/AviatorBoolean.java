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
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * Aviator boolean type
 * 
 * @author dennis
 * 
 */
public class AviatorBoolean extends AviatorObject {

  Boolean value;

  public static final AviatorBoolean TRUE = new AviatorBoolean(Boolean.TRUE);

  public static final AviatorBoolean FALSE = new AviatorBoolean(Boolean.FALSE);


  @Override
  public AviatorObject not(Map<String, Object> env) {
    return this.value.booleanValue() ? FALSE : TRUE;
  }


  @Override
  public final boolean booleanValue(Map<String, Object> env) {
    return value.booleanValue();
  }


  @Override
  public AviatorObject add(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case String:
        return new AviatorString(this.value.toString() + ((AviatorString) other).lexeme);
      case JavaType:
        AviatorJavaType javaType = (AviatorJavaType) other;
        final Object otherJavaValue = javaType.getValue(env);
        if (TypeUtils.isString(otherJavaValue)) {
          return new AviatorString(this.value.toString() + otherJavaValue.toString());
        } else {
          return super.add(other, env);
        }
      default:
        return super.add(other, env);
    }

  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Boolean;
  }


  @Override
  public final Object getValue(Map<String, Object> env) {
    return this.value;
  }


  private AviatorBoolean(Boolean value) {
    super();
    this.value = value;
  }


  public static AviatorBoolean valueOf(boolean b) {
    return b ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
  }


  @Override
  public int compare(AviatorObject other, Map<String, Object> env) {
    if (this == other)
      return 0;
    switch (other.getAviatorType()) {
      case Boolean:
        AviatorBoolean otherBoolean = (AviatorBoolean) other;
        return this.value.compareTo(otherBoolean.value);
      case JavaType:
        AviatorJavaType javaType = (AviatorJavaType) other;
        final Object otherValue = javaType.getValue(env);
        if (otherValue == null) {
          return 1;
        }
        if (otherValue instanceof Boolean) {
          return this.value.compareTo((Boolean) otherValue);
        } else {
          throw new ExpressionRuntimeException(
              "Could not compare " + desc(env) + " with " + other.desc(env));
        }
      case Nil:
        return 1;
      default:
        throw new ExpressionRuntimeException(
            "Could not compare " + this.desc(env) + " with " + other.desc(env));
    }

  }

}
