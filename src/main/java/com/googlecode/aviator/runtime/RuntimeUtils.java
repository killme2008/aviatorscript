package com.googlecode.aviator.runtime;

import java.io.IOException;
import java.math.MathContext;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
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
  public static final AviatorEvaluatorInstance getInstance(Map<String, Object> env) {
    return ((Env) env).getInstance();

  }

  public static final MathContext getMathContext(Map<String, Object> env) {
    return getInstance(env).getOption(Options.MATH_CONTEXT);
  }

  public static final void printTrace(Map<String, Object> env, String msg) {
    try {
      getInstance(env).getTraceOutputStream().write(("[Aviator TRACE] " + msg + "\n").getBytes());
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public static final boolean isTracedEval(Map<String, Object> env) {
    return (boolean) getInstance(env).getOption(Options.TRACE_EVAL);
  }


  public static AviatorFunction getFunction(Object object, Map<String, Object> env) {
    if (object instanceof AviatorFunction) {
      return (AviatorFunction) object;
    } else if (object instanceof AviatorObject) {
      Object value = ((AviatorObject) object).getValue(env);
      if (value instanceof AviatorFunction) {
        return (AviatorFunction) value;
      }
    }
    throw new ExpressionRuntimeException(
        "Could not cast object " + object + " into a aviator function.");
  }

  public static AviatorFunction getFunction(Map<String, Object> env, String name) {
    return getInstance(env).getFunction(name);
  }

}
