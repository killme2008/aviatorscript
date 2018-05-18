package com.googlecode.aviator.runtime;

import java.io.IOException;
import java.math.MathContext;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

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
  public static final AviatorEvaluatorInstance getInstance() {
    AviatorEvaluatorInstance instance = RuntimeUtils.INSTANCE.get();
    return instance != null ? instance : AviatorEvaluator.getInstance();
  }

  public static final MathContext getMathContext() {
    return getInstance().getOption(Options.MATH_CONTEXT);
  }

  public static final void printTrace(String msg) {
    try {
      getInstance().getTraceOutputStream().write(("[Aviator TRACE] " + msg + "\n").getBytes());
    } catch (IOException e) {
      e.printStackTrace();
    }
  }

  public static final boolean isTracedEval() {
    return (boolean) getInstance().getOption(Options.TRACE_EVAL);
  }

  public static final void setInstance(AviatorEvaluatorInstance instance) {
    if (instance != AviatorEvaluator.getInstance()) {
      RuntimeUtils.INSTANCE.set(instance);
    }
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

  public static AviatorFunction getFunction(String name) {
    return getInstance().getFunction(name);
  }

  public static void removeInstance(AviatorEvaluatorInstance instance) {
    if (instance != AviatorEvaluator.getInstance()) {
      RuntimeUtils.INSTANCE.remove();
    }
  }

  public static final ThreadLocal<AviatorEvaluatorInstance> INSTANCE =
      new ThreadLocal<AviatorEvaluatorInstance>();

}
