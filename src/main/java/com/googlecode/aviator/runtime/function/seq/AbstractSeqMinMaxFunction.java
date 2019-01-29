package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * Base class for min/max function.
 *
 * @author dennis
 *
 */
public abstract class AbstractSeqMinMaxFunction extends AbstractFunction {

  static enum Op {
    Min, Max
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    Object first = arg1.getValue(env);
    if (first == null) {
      return AviatorNil.NIL;
    }
    Class<?> clazz = first.getClass();

    boolean wasFirst = true;

    if (Collection.class.isAssignableFrom(clazz)) {
      Collection<?> list = (Collection<?>) first;

      if (list.isEmpty()) {
        return AviatorNil.NIL;
      }

      Object result = null;
      for (Object obj : list) {
        result = compareObjects(result, obj, wasFirst);
        if (wasFirst) {
          wasFirst = false;
        }
        if (getOp() == Op.Min && result == null) {
          break;
        }
      }

      return AviatorRuntimeJavaType.valueOf(result);
    } else if (clazz.isArray()) {
      int length = Array.getLength(first);

      if (length == 0) {
        return AviatorNil.NIL;
      }


      Object result = null;
      for (int i = 0; i < length; i++) {
        Object obj = Array.get(first, i);
        result = compareObjects(result, obj, wasFirst);
        if (wasFirst) {
          wasFirst = false;
        }
        if (getOp() == Op.Min && result == null) {
          break;
        }
      }
      return AviatorRuntimeJavaType.valueOf(result);
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a seq");
    }
  }

  protected abstract Op getOp();

  private Object compareObjects(Object result, Object obj, boolean wasFirst) {
    if (obj == null) {
      switch (getOp()) {
        case Min:
          return obj;
        case Max:
          return result;
      }
    }
    if (!(obj instanceof Comparable)) {
      throw new IllegalArgumentException(
          "Element in sequence doesn't implement java.lang.Comparable.");
    }
    if (wasFirst || compare(result, obj)) {
      result = obj;
    }
    return result;
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  private boolean compare(Object result, Object obj) {
    try {
      int c = ((Comparable) obj).compareTo(result);
      switch (getOp()) {
        case Min:
          return c < 0;
        case Max:
          return c > 0;
      }
      return false;
    } catch (RuntimeException e) {
      throw new ExpressionRuntimeException("Could not compare `" + obj + "` with `" + result + "`",
          e);
    }
  }



}
