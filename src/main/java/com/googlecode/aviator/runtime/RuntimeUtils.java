package com.googlecode.aviator.runtime;

import java.io.IOException;
import java.math.MathContext;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.Sequence;
import com.googlecode.aviator.runtime.type.seq.ArraySequence;
import com.googlecode.aviator.runtime.type.seq.CharSeqSequence;
import com.googlecode.aviator.runtime.type.seq.IterableSequence;
import com.googlecode.aviator.runtime.type.seq.MapSequence;
import com.googlecode.aviator.utils.Env;

/**
 * Runtime utils
 *
 * @author dennis
 *
 */
public final class RuntimeUtils {

  private RuntimeUtils() {

  }

  /**
   * Get the current evaluator instance,returns the global instance if not found.
   *
   * @return
   */
  public static final AviatorEvaluatorInstance getInstance(final Map<String, Object> env) {
    if (env instanceof Env) {
      return ((Env) env).getInstance();
    }
    return AviatorEvaluator.getInstance();

  }

  /**
   * Cast an object into sequence if possible, otherwise throw an exception.
   *
   * @param o
   * @return
   */
  @SuppressWarnings({"rawtypes", "unchecked"})
  public static Sequence seq(final Object o) {
    if (o instanceof Sequence) {
      return (Sequence) o;
    } else if (o instanceof CharSequence) {
      return new CharSeqSequence((CharSequence) o);
    } else if (o instanceof Iterable) {
      return new IterableSequence((Iterable) o);
    } else if (o.getClass().isArray()) {
      return new ArraySequence(o);
    } else if (o instanceof Map) {
      return new MapSequence((Map) o);
    } else {
      throw new IllegalArgumentException(o + " is not a sequence");
    }
  }

  /**
   * Ensure the object is not null, cast null into AviatorNil.
   *
   * @param object
   * @return
   */
  public static final AviatorObject assertNotNull(final AviatorObject object) {
    if (object != null) {
      return object;
    }
    return AviatorNil.NIL;
  }

  public static final MathContext getMathContext(final Map<String, Object> env) {
    return getInstance(env).getOptionValue(Options.MATH_CONTEXT).mathContext;
  }


  public static final void printlnTrace(final Map<String, Object> env, final String msg) {
    try {
      getInstance(env).getTraceOutputStream().write(("[Aviator TRACE] " + msg + "\n").getBytes());
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public static final boolean isTracedEval(final Map<String, Object> env) {
    return getInstance(env).getOptionValue(Options.TRACE_EVAL).bool;
  }

  public static AviatorFunction getFunction(final Object object, final Map<String, Object> env) {
    if (object instanceof AviatorFunction) {
      return (AviatorFunction) object;
    } else if (object instanceof AviatorObject) {
      Object value = ((AviatorObject) object).getValue(env);
      if (value instanceof AviatorFunction) {
        return (AviatorFunction) value;
      }
    }
    throw new ClassCastException("Could not cast object " + object + " into a aviator function.");
  }

  public static AviatorFunction getFunction(final Map<String, Object> env, final String name) {
    return getInstance(env).getFunction(name);
  }

  public static void printStackTrace(final Map<String, Object> env, final Exception e) {
    if (isTracedEval(env)) {
      e.printStackTrace();
    }
  }
}
