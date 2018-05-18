package com.googlecode.aviator.runtime;

import java.io.IOException;
import java.math.MathContext;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;

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

  public static void removeInstance(AviatorEvaluatorInstance instance) {
    if (instance != AviatorEvaluator.getInstance()) {
      RuntimeUtils.INSTANCE.remove();
    }
  }

  public static final ThreadLocal<AviatorEvaluatorInstance> INSTANCE =
      new ThreadLocal<AviatorEvaluatorInstance>();

}
