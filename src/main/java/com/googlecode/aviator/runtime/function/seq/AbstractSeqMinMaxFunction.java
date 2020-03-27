package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.Sequence;

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

  @SuppressWarnings("rawtypes")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    Object first = arg1.getValue(env);
    if (first == null) {
      return AviatorNil.NIL;
    }


    Sequence seq = RuntimeUtils.seq(first, env);

    boolean wasFirst = true;

    Object result = null;
    for (Object obj : seq) {
      result = compareObjects(result, obj, wasFirst);
      if (wasFirst) {
        wasFirst = false;
      }
      if (getOp() == Op.Min && result == null) {
        break;
      }
    }

    return AviatorRuntimeJavaType.valueOf(result);
  }

  protected abstract Op getOp();

  private Object compareObjects(Object result, final Object obj, final boolean wasFirst) {
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
  private boolean compare(final Object result, final Object obj) {
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
