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
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * Aviator root object
 *
 * @author dennis
 *
 */
public abstract class AviatorObject {
  public abstract int compare(AviatorObject other, Map<String, Object> env);


  public abstract AviatorType getAviatorType();


  @Override
  public String toString() {
    return desc(Env.EMPTY_ENV);
  }


  /**
   * Returns true if the aviator object is null.
   *
   * @since 3.0.0
   * @return
   */
  public boolean isNull(final Map<String, Object> env) {
    return getValue(env) == null;
  }


  public AviatorObject match(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(desc(env) + " doesn't support match operation '=~'");
  }


  public AviatorObject neg(final Map<String, Object> env) {
    throw new ExpressionRuntimeException(desc(env) + " doesn't support negative operation '-'");
  }


  public AviatorObject not(final Map<String, Object> env) {
    throw new ExpressionRuntimeException(desc(env) + " doesn't support not operation '!'");
  }


  public String desc(final Map<String, Object> env) {
    return "<" + getAviatorType() + ", " + getValue(env) + ">";
  }


  public abstract Object getValue(Map<String, Object> env);


  public AviatorObject add(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException("Could not add " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject bitAnd(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitAnd " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject bitOr(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitOr " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject bitXor(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not bitXor " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject shiftRight(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not shiftRight " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject shiftLeft(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not shiftLeft " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject unsignedShiftRight(final AviatorObject other,
      final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not unsignedShiftRight " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject bitNot(final Map<String, Object> env) {
    throw new ExpressionRuntimeException(desc(env) + " doesn't support not operation '^'");
  }


  public AviatorObject sub(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException("Could not sub " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject mod(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException("Could not mod " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject div(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException("Could not div " + desc(env) + " with " + other.desc(env));
  }


  public AviatorObject mult(final AviatorObject other, final Map<String, Object> env) {
    throw new ExpressionRuntimeException(
        "Could not mult " + desc(env) + " with " + other.desc(env));
  }


  public Number numberValue(final Map<String, Object> env) {
    if (!(getValue(env) instanceof Number)) {
      throw new ExpressionRuntimeException(desc(env) + " is not a number value");
    }
    return (Number) getValue(env);
  }


  public String stringValue(final Map<String, Object> env) {
    Object value = getValue(env);
    if (!(TypeUtils.isString(value))) {
      throw new ExpressionRuntimeException(desc(env) + " is not a string value");
    }
    return String.valueOf(value);
  }


  public boolean booleanValue(final Map<String, Object> env) {
    if (!(getValue(env) instanceof Boolean)) {
      throw new ExpressionRuntimeException(desc(env) + " is not a boolean value");
    }
    return (Boolean) getValue(env);
  }


  /**
   * Access array or list element
   *
   * @param env
   * @param indexObject
   * @return
   */
  public AviatorObject getElement(final Map<String, Object> env, final AviatorObject indexObject) {
    throw new ExpressionRuntimeException(desc(env) + " is not a array");
  }
}
