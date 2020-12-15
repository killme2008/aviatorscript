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

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.regex.Pattern;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.CompareNotSupportedException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.DispatchFunction;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaElementType.ContainerType;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.Reflector.PropertyType;
import com.googlecode.aviator.utils.TypeUtils;

/**
 * Aviator variable
 *
 * @author dennis
 *
 */
public class AviatorJavaType extends AviatorObject {
  private static final long serialVersionUID = -4353225521490659987L;
  private String name;
  private final boolean containsDot;
  private String[] subNames;
  // slow path to get nested property
  private boolean slowPath = false;

  @Override
  public AviatorType getAviatorType() {
    return AviatorType.JavaType;
  }

  public String getName() {
    return this.name;
  }

  public AviatorJavaType(final String name) {
    this(name, null);
  }

  public AviatorJavaType(final String name, final SymbolTable symbolTable) {
    super();
    String rName = reserveName(name);
    if (rName != null) {
      this.name = rName;
    } else {
      if (symbolTable != null) {
        this.name = symbolTable.reserve(name).getLexeme();
      } else {
        this.name = name;
      }
    }
    this.containsDot = this.name.contains(".");
  }

  /**
   * Reserved special var names, return null if not successes.
   *
   * @param name
   * @return
   */
  private static String reserveName(final String name) {
    if (Constants.ENV_VAR.equals(name)) {
      return Constants.ENV_VAR;
    } else if (Constants.ReducerEmptyVal.getLexeme().equals(name)) {
      return Constants.ReducerEmptyVal.getLexeme();
    } else if (Constants.FUNC_ARGS_VAR.equals(name)) {
      return Constants.FUNC_ARGS_VAR;
    } else if (Constants.REDUCER_LOOP_VAR.equals(name)) {
      return Constants.REDUCER_LOOP_VAR;
    } else if (Constants.INSTANCE_VAR.equals(name)) {
      return Constants.INSTANCE_VAR;
    } else if (Constants.EXP_VAR.equals(name)) {
      return Constants.EXP_VAR;
    } else {
      return null;
    }
  }

  @Override
  public AviatorObject deref(final Map<String, Object> env) {
    return AviatorRuntimeJavaType.valueOf(getValue(env));
  }

  @Override
  public AviatorObject div(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.div(other, env);
        } else {
          return super.div(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.div(other, env);
        } else {
          return super.div(other, env);
        }
      default:
        return super.div(other, env);
    }
  }



  @Override
  public AviatorObject match(final AviatorObject other, final Map<String, Object> env) {
    Object val = getValue(env);
    if (val instanceof Pattern) {
      return new AviatorPattern((Pattern) val).match(other, env);
    } else {
      return super.match(other, env);
    }
  }

  @Override
  public AviatorObject bitAnd(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.bitAnd(other, env);
        } else {
          return super.bitAnd(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.bitAnd(other, env);
        } else {
          return super.bitAnd(other, env);
        }
      default:
        return super.bitAnd(other, env);
    }
  }

  @Override
  public AviatorObject bitNot(final Map<String, Object> env) {
    final Object value = getValue(env);
    if (value instanceof Number) {
      return AviatorNumber.valueOf(value).bitNot(env);
    } else {
      return super.bitNot(env);
    }
  }

  @Override
  public AviatorObject bitOr(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.bitOr(other, env);
        } else {
          return super.bitOr(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.bitOr(other, env);
        } else {
          return super.bitOr(other, env);
        }
      default:
        return super.bitOr(other, env);
    }
  }

  @Override
  public AviatorObject bitXor(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.bitXor(other, env);
        } else {
          return super.bitXor(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.bitXor(other, env);
        } else {
          return super.bitXor(other, env);
        }
      default:
        return super.bitXor(other, env);
    }
  }

  @Override
  public AviatorObject shiftLeft(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.shiftLeft(other, env);
        } else {
          return super.shiftLeft(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.shiftLeft(other, env);
        } else {
          return super.shiftLeft(other, env);
        }
      default:
        return super.shiftLeft(other, env);
    }
  }

  @Override
  public AviatorObject shiftRight(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.shiftRight(other, env);
        } else {
          return super.shiftRight(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.shiftRight(other, env);
        } else {
          return super.shiftRight(other, env);
        }
      default:
        return super.shiftRight(other, env);
    }
  }

  @Override
  public AviatorObject unsignedShiftRight(final AviatorObject other,
      final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.unsignedShiftRight(other, env);
        } else {
          return super.unsignedShiftRight(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.unsignedShiftRight(other, env);
        } else {
          return super.unsignedShiftRight(other, env);
        }
      default:
        return super.unsignedShiftRight(other, env);
    }
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    return this.getValueFromEnv(this.name, this.containsDot, env, true);
  }

  public Object getValueFromEnv(final String name, final boolean nameContainsDot,
      final Map<String, Object> env, final boolean throwExceptionNotFound) {
    if (env != null) {
      if (nameContainsDot && !env.containsKey(name) && RuntimeUtils.getInstance(env)
          .getOptionValue(Options.ENABLE_PROPERTY_SYNTAX_SUGAR).bool) {
        if (this.subNames == null) {
          // cache the result
          this.subNames = SPLIT_PAT.split(name);
        }
        return getProperty(name, this.subNames, env, throwExceptionNotFound, this, false);
      }
      return env.get(name);
    }
    return null;
  }

  public static Object getValueFromEnv(final String name, final boolean nameContainsDot,
      final String[] names, final Map<String, Object> env, final boolean throwExceptionNotFound,
      final boolean tryResolveStaticMethod) {
    if (env != null) {
      if (nameContainsDot && RuntimeUtils.getInstance(env)
          .getOptionValue(Options.ENABLE_PROPERTY_SYNTAX_SUGAR).bool) {
        return getProperty(name, names, env, throwExceptionNotFound, null, tryResolveStaticMethod);
      }
      return env.get(name);
    }
    return null;
  }


  @Override
  public AviatorObject defineValue(final AviatorObject value, final Map<String, Object> env) {
    if (this.containsDot) {
      return setProperty(value, env);
    }

    Object v = getAssignedValue(value, env);

    // TODO refactor
    // special processing for define functions.
    if (v instanceof LambdaFunction) {
      // try to define a function
      Object existsFn = getValue(env);
      if (existsFn instanceof DispatchFunction) {
        // It's already an overload function, install the new branch.
        ((DispatchFunction) existsFn).install((LambdaFunction) v);
        return AviatorRuntimeJavaType.valueOf(existsFn);
      } else if (existsFn instanceof LambdaFunction) {
        // cast it to an overload function
        DispatchFunction newFn = new DispatchFunction(this.name);
        // install the exists branch
        newFn.install((LambdaFunction) existsFn);
        // and the new branch.
        newFn.install(((LambdaFunction) v));
        v = newFn;
      } else if (existsFn == null && ((LambdaFunction) v).isVariadic()) {
        // cast variadic function to overload function
        DispatchFunction newFn = new DispatchFunction(this.name);
        newFn.install(((LambdaFunction) v));
        v = newFn;
      }
    }

    ((Env) env).override(this.name, v);
    return AviatorRuntimeJavaType.valueOf(v);
  }

  private Object getAssignedValue(final AviatorObject value, final Map<String, Object> env) {
    Object v = value.getValue(env);
    if (v instanceof AviatorObject) {
      v = ((AviatorObject) v).deref(env);
    }
    return v;
  }

  @Override
  public AviatorObject setValue(final AviatorObject value, final Map<String, Object> env) {
    if (this.containsDot) {
      return setProperty(value, env);
    }

    Object v = getAssignedValue(value, env);
    env.put(this.name, v);
    return AviatorRuntimeJavaType.valueOf(v);
  }

  private AviatorObject setProperty(final AviatorObject value, final Map<String, Object> env) {
    if (RuntimeUtils.getInstance(env).getOptionValue(Options.ENABLE_PROPERTY_SYNTAX_SUGAR).bool) {
      Object v = value.getValue(env);
      try {
        Reflector.setProperty(env, this.name, value.getValue(env));
      } catch (Throwable t) {
        if (RuntimeUtils.getInstance(env).getOptionValue(Options.TRACE_EVAL).bool) {
          t.printStackTrace();
        }
        throw new ExpressionRuntimeException("Can't assign value to " + this.name, t);
      }
      return AviatorRuntimeJavaType.valueOf(v);
    } else {
      throw new ExpressionRuntimeException("Can't assign value to " + this.name
          + ", Options.ENABLE_PROPERTY_SYNTAX_SUGAR is disabled.");
    }
  }

  public static final Pattern SPLIT_PAT = Pattern.compile("\\.");

  private static Object getProperty(final String name, String[] names,
      final Map<String, Object> env, final boolean throwExceptionNotFound,
      final AviatorJavaType javaType, final boolean tryResolveStaticMethod) {
    try {
      if (names == null) {
        names = SPLIT_PAT.split(name);
      }
      return fastGetProperty(name, names, env, javaType, tryResolveStaticMethod);

    } catch (Throwable t) {
      if (RuntimeUtils.getInstance(env).getOptionValue(Options.TRACE_EVAL).bool) {
        t.printStackTrace();
      }
      if (RuntimeUtils.getInstance(env).getOptionValue(Options.NIL_WHEN_PROPERTY_NOT_FOUND).bool) {
        return null;
      } else if (throwExceptionNotFound) {
        throw new ExpressionRuntimeException("Could not find variable " + name, t);
      } else {
        return null;
      }
    }
  }

  @SuppressWarnings("unchecked")
  private static Object fastGetProperty(final String name, final String[] names,
      final Map<String, Object> env, final AviatorJavaType javaType,
      final boolean tryResolveStaticMethod)
      throws NoSuchMethodException, InvocationTargetException, IllegalAccessException {
    if (javaType != null && javaType.slowPath) {
      return Reflector.getProperty(env, name);
    }

    Map<String, Object> innerEnv = env;
    Class<?> innerClazz = null;
    Object targetObject = null;
    for (int i = 0; i < names.length; i++) {
      String rName = reserveName(names[i]);
      rName = rName != null ? rName : names[i];
      int arrayIndex = -1;
      String keyIndex = null;

      // compatible with PropertyUtilsBean indexed and mapped formats.
      // https://commons.apache.org/proper/commons-beanutils/apidocs/org/apache/commons/beanutils/PropertyUtilsBean.html
      switch (rName.charAt(rName.length() - 1)) {
        case ']':
          int idx = rName.indexOf("[");
          if (idx < 0) {
            throw new IllegalArgumentException("Should not happen, doesn't contains '['");
          }
          String rawName = rName;
          rName = rName.substring(0, idx);
          arrayIndex = Integer.valueOf(rawName.substring(idx + 1, rawName.length() - 1));
          break;
        case ')':
          idx = rName.indexOf("(");
          if (idx < 0) {
            throw new IllegalArgumentException("Should not happen, doesn't contains '('");
          }
          rawName = rName;
          rName = rName.substring(0, idx);
          keyIndex = rawName.substring(idx + 1, rawName.length() - 1);
          break;
      }


      Object val = null;
      if (innerEnv != null) {
        val = innerEnv.get(rName);
        if (val == null && i == 0 && env instanceof Env) {
          val = tryResolveAsClass(env, rName);
        }
      } else if (innerClazz != null) {
        final AviatorEvaluatorInstance instance = RuntimeUtils.getInstance(env);
        if (tryResolveStaticMethod && instance.isFeatureEnabled(Feature.StaticMethods)
            && names.length == 2) {
          val = Reflector.fastGetProperty(innerClazz, rName, PropertyType.StaticMethod);
        } else if (instance.isFeatureEnabled(Feature.StaticFields)) {
          val = Reflector.fastGetProperty(innerClazz, rName, PropertyType.StaticField);
        } else {
          val = Reflector.fastGetProperty(innerClazz, rName, PropertyType.Getter);
        }
      } else {
        // in the format of a.b.[0].c
        if (rName.isEmpty()) {
          if (!(arrayIndex >= 0 || keyIndex != null)) {
            throw new IllegalArgumentException("Invalid format");
          }
          val = targetObject;
        } else {
          val = Reflector.fastGetProperty(targetObject, rName, PropertyType.Getter);
        }
      }

      if (arrayIndex >= 0) {
        if (val.getClass().isArray()) {
          val = Array.get(val, arrayIndex);
        } else if (val instanceof List) {
          val = ((List) val).get(arrayIndex);
        } else if (val instanceof CharSequence) {
          val = ((CharSequence) val).charAt(arrayIndex);
        } else {
          throw new IllegalArgumentException("Can't access " + val + " with index `" + arrayIndex
              + "`, it's not an array, list or CharSequence");
        }
      }
      if (keyIndex != null) {
        if (Map.class.isAssignableFrom(val.getClass())) {
          val = ((Map) val).get(keyIndex);
        } else {
          throw new IllegalArgumentException(
              "Can't access " + val + " with key `" + keyIndex + "`, it's not a map");
        }
      }

      if (i == names.length - 1) {
        return val;
      }
      if (val instanceof Map) {
        innerEnv = (Map<String, Object>) val;
        innerClazz = null;
        targetObject = null;
      } else if (val instanceof Class<?>) {
        innerClazz = (Class<?>) val;
        innerEnv = null;
        targetObject = null;
      } else if (val == null) {
        throw new NullPointerException(rName);
      } else {
        targetObject = val;
        innerEnv = null;
        innerClazz = null;
      }
    }
    if (javaType != null) {
      javaType.slowPath = true;
    }
    return Reflector.getProperty(env, name);
  }

  private static Object tryResolveAsClass(final Map<String, Object> env, final String rName) {
    try {
      return ((Env) env).resolveClassSymbol(rName);
    } catch (ClassNotFoundException e) {
      return null;
    }
  }

  @Override
  public AviatorObject mod(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.mod(other, env);
        } else {
          return super.mod(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.mod(other, env);
        } else {
          return super.mod(other, env);
        }
      default:
        return super.mod(other, env);
    }
  }

  @Override
  public AviatorObject sub(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.sub(other, env);
        } else {
          return super.sub(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.sub(other, env);
        } else {
          return super.sub(other, env);
        }
      default:
        return super.sub(other, env);
    }
  }

  @Override
  @SuppressWarnings("unchecked")
  public int innerCompare(final AviatorObject other, final Map<String, Object> env) {
    if (this == other) {
      return 0;
    }
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        AviatorNumber aviatorNumber = (AviatorNumber) other;
        return -aviatorNumber.compare(this, env);
      case String:
        AviatorString aviatorString = (AviatorString) other;
        return -aviatorString.compare(this, env);
      case Boolean:
        AviatorBoolean aviatorBoolean = (AviatorBoolean) other;
        return -aviatorBoolean.compare(this, env);
      case JavaType:

        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object thisValue = getValue(env);
        final Object otherValue = otherJavaType.getValue(env);
        if (thisValue == null) {
          return AviatorNil.NIL.compare(other, env);
        }
        if (thisValue.equals(otherValue)) {
          return 0;
        } else {
          if (thisValue instanceof Number) {
            AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(thisValue);
            return thisAviatorNumber.compare(other, env);
          } else if (TypeUtils.isString(thisValue)) {
            AviatorString thisAviatorString = new AviatorString(String.valueOf(thisValue));
            return thisAviatorString.compare(other, env);
          } else if (thisValue instanceof Boolean) {
            AviatorBoolean thisAviatorBoolean = AviatorBoolean.valueOf((Boolean) thisValue);
            return thisAviatorBoolean.compare(other, env);
          } else if (thisValue instanceof Date && otherValue instanceof String) {
            // This is date,other is string
            return tryCompareDate(thisValue, otherValue);
          } else if (otherValue == null) {
            return 1;
          } else {
            try {
              return ((Comparable<Object>) thisValue).compareTo(otherValue);
            } catch (ClassCastException e) {
              throw new CompareNotSupportedException(
                  "Compare " + desc(env) + " with " + other.desc(env) + " error", e);
            }
          }
        }
      case Nil:
        // Any object is greater than nil except nil
        if (getValue(env) == null) {
          return 0;
        } else {
          return 1;
        }
      default:
        throw new CompareNotSupportedException("Unknow aviator type");
    }
  }

  private int tryCompareDate(final Object thisValue, final Object otherValue) {
    try {
      SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS");
      Date otherDate = simpleDateFormat.parse((String) otherValue);
      return ((Date) thisValue).compareTo(otherDate);
    } catch (Throwable t) {
      throw new ExpressionRuntimeException("Compare date error", t);
    }
  }

  @Override
  public AviatorObject mult(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        if (value instanceof Number) {
          AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
          return aviatorNumber.mult(other, env);
        } else {
          return super.mult(other, env);
        }
      case JavaType:
        if (value instanceof Number) {
          AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
          return thisAviatorNumber.mult(other, env);
        } else {
          return super.mult(other, env);
        }
      default:
        return super.mult(other, env);
    }
  }

  @Override
  public AviatorObject neg(final Map<String, Object> env) {
    final Object value = getValue(env);
    if (value instanceof Number) {
      return AviatorNumber.valueOf(value).neg(env);
    } else {
      return super.neg(env);
    }
  }

  @Override
  public AviatorObject not(final Map<String, Object> env) {
    final Object value = getValue(env);
    if (value instanceof Boolean) {
      return AviatorBoolean.valueOf((Boolean) value).not(env);
    } else {
      return super.not(env);
    }
  }

  /**
   * Access array or list element
   *
   * @param env
   * @param indexObject
   * @return
   */
  @Override
  public AviatorObject getElement(final Map<String, Object> env, final AviatorObject indexObject) {
    final Object thisValue = getValue(env);
    final Object indexValue = indexObject.getValue(env);

    if (thisValue.getClass().isArray()) {
      final int index = ((Number) indexValue).intValue();
      return new AviatorRuntimeJavaElementType(ContainerType.Array, thisValue, index,
          new Callable<Object>() {
            @Override
            public Object call() throws Exception {
              return Array.get(thisValue, index);
            }
          });
    } else if (thisValue instanceof List) {
      final int index = ((Number) indexValue).intValue();
      return new AviatorRuntimeJavaElementType(ContainerType.List, thisValue, index,
          new Callable<Object>() {

            @Override
            public Object call() throws Exception {
              return ((List<?>) thisValue).get(index);
            }

          });
    } else if (thisValue instanceof Map) {
      return new AviatorRuntimeJavaElementType(ContainerType.Map, thisValue, indexValue,
          new Callable<Object>() {

            @Override
            public Object call() throws Exception {
              return ((Map) thisValue).get(indexValue);
            }

          });
    } else {
      throw new ExpressionRuntimeException(
          desc(env) + " is not an array, list or map,could not use [] to get element");
    }
  }

  @Override
  public AviatorObject add(final AviatorObject other, final Map<String, Object> env) {
    final Object value = getValue(env);
    Object otherValue;
    if (value instanceof Number) {
      AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
      return aviatorNumber.add(other, env);
    } else if (TypeUtils.isString(value)) {
      AviatorString aviatorString = new AviatorString(String.valueOf(value));
      return aviatorString.add(other, env);
    } else if (value instanceof Boolean) {
      return AviatorBoolean.valueOf((Boolean) value).add(other, env);
    } else if (value == null && (otherValue = other.getValue(env)) instanceof CharSequence) {
      return new AviatorString("null" + otherValue);
    } else {
      return super.add(other, env);
    }
  }

  @Override
  public String desc(final Map<String, Object> env) {
    Object value = getValue(env);
    return "<" + getAviatorType() + ", " + this.name + ", " + value + ", "
        + (value == null ? "null" : value.getClass().getName()) + ">";
  }

}
