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

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.BaseExpression;
import com.googlecode.aviator.code.asm.ASMCodeGenerator;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;
import com.googlecode.aviator.runtime.type.AviatorType;


/**
 * Function helper
 *
 * @author dennis
 *
 */
public class FunctionUtils {

  /**
   * Retrieve the invocation arguments info from env, returns null when absent.
   *
   * @param env
   * @return
   */
  @SuppressWarnings("unchecked")
  public static List<FunctionArgument> getFunctionArguments(final Map<String, Object> env) {
    Map<Integer, List<FunctionArgument>> funcParams =
        (Map<Integer, List<FunctionArgument>>) env.get(BaseExpression.FUNC_PARAMS_VAR);
    if (funcParams == null) {
      return null;
    }
    Integer refId = (Integer) env.get(ASMCodeGenerator.FUNC_ARGS_INNER_VAR);
    if (refId == null) {
      return null;
    }
    return funcParams.get(refId);
  }


  /**
   * Get string value from env.
   *
   * @param arg the var name
   * @param env
   * @return
   */
  public static final String getStringValue(final AviatorObject arg,
      final Map<String, Object> env) {
    String result = null;

    final Object value = arg.getValue(env);
    if (value instanceof Character) {
      result = value.toString();
    } else {
      result = (String) value;
    }
    return result;
  }

  /**
   * get a object from env
   *
   * @param arg the var name
   * @param env
   * @return
   */
  public static Object getJavaObject(final AviatorObject arg, final Map<String, Object> env) {
    if (arg.getAviatorType() != AviatorType.JavaType) {
      throw new ClassCastException(arg.desc(env) + " is not a javaType");
    }
    return ((AviatorJavaType) arg).getValue(env);
  }


  /**
   * Get a function from env in follow orders:
   * <ul>
   * <li>arg value</li>
   * <li>env</li>
   * <li>current evaluator instance.</li>
   * </ul>
   *
   * @param arg
   * @param env
   * @param arity
   * @return
   */
  public static AviatorFunction getFunction(final AviatorObject arg, final Map<String, Object> env,
      final int arity) {
    if (arg.getAviatorType() != AviatorType.JavaType
        && arg.getAviatorType() != AviatorType.Lambda) {
      throw new ClassCastException(arg.desc(env) + " is not a function");
    }
    // Runtime type.
    if (arg instanceof AviatorRuntimeJavaType && arg.getValue(env) instanceof AviatorFunction) {
      return (AviatorFunction) arg.getValue(env);
    }
    if (arg instanceof AviatorFunction) {
      return (AviatorFunction) arg;
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
      AviatorEvaluatorInstance instance = RuntimeUtils.getInstance(env);
      rt = instance.getFunction(name);
    }
    return rt;
  }


  /**
   * Get a number from env.
   *
   * @param arg1 the var
   * @param env
   * @return
   */
  public static final Number getNumberValue(final AviatorObject arg1,
      final Map<String, Object> env) {
    return (Number) arg1.getValue(env);
  }


  /**
   * Wraps the object as aviator object.
   *
   * @since 4.2.5
   * @param ret the java object
   * @return wrapped aviator object
   */
  public static AviatorObject wrapReturn(final Object ret) {
    if (ret == null) {
      return AviatorNil.NIL;
    } else if (ret instanceof Number) {
      return AviatorNumber.valueOf(ret);
    } else if (ret instanceof CharSequence) {
      return new AviatorString(ret.toString());
    } else if (ret instanceof Boolean) {
      return AviatorBoolean.valueOf((boolean) ret);
    } else if (ret instanceof AviatorObject) {
      return (AviatorObject) ret;
    } else {
      return new AviatorRuntimeJavaType(ret);
    }
  }

}
