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
package com.googlecode.aviator.runtime.function;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;


/**
 * Function helper
 * 
 * @author dennis
 * 
 */
public class FunctionUtils {

  public static final String getStringValue(AviatorObject arg, Map<String, Object> env) {
    String result = null;

    final Object value = arg.getValue(env);
    if (value == null && arg.getAviatorType() == AviatorType.JavaType) {
      throw new NullPointerException(
          "There is no string named" + ((AviatorJavaType) arg).getName());
    }
    if (value instanceof Character) {
      result = value.toString();
    } else {
      result = (String) value;
    }
    return result;
  }


  public static Object getJavaObject(AviatorObject arg, Map<String, Object> env) {
    if (arg.getAviatorType() != AviatorType.JavaType) {
      throw new ExpressionRuntimeException(arg.desc(env) + " is not a javaType");
    }
    return env.get(((AviatorJavaType) arg).getName());
  }


  public static AviatorFunction getFunction(AviatorObject arg, Map<String, Object> env, int arity) {
    if (arg.getAviatorType() != AviatorType.JavaType) {
      throw new ExpressionRuntimeException(arg.desc(env) + " is not a function");
    }
    // Runtime type.
    if (arg instanceof AviatorRuntimeJavaType && arg.getValue(env) instanceof AviatorFunction) {
      return (AviatorFunction) arg.getValue(env);
    }
    // resolve by name.
    // special processing for "-" operator
    String name = ((AviatorJavaType) arg).getName();
    if (name.equals("-")) {
      if (arity == 2) {
        name = "-sub";
      } else {
        name = "-neg";
      }
    }
    AviatorFunction rt = null;
    if (env != null) {
      rt = (AviatorFunction) env.get(name);
    }
    if (rt == null) {
      rt = AviatorEvaluator.getFunction(name);
    }
    return rt;
  }


  public static final Number getNumberValue(AviatorObject arg1, Map<String, Object> env) {
    return (Number) arg1.getValue(env);
  }

}
