package com.googlecode.aviator.runtime;

import java.io.IOException;
import java.math.MathContext;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.TimeoutException;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.runtime.function.internal.UnpackingArgsFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.Sequence;
import com.googlecode.aviator.runtime.type.seq.ArraySequence;
import com.googlecode.aviator.runtime.type.seq.CharSeqSequence;
import com.googlecode.aviator.runtime.type.seq.EmptySequence;
import com.googlecode.aviator.runtime.type.seq.IterableSequence;
import com.googlecode.aviator.runtime.type.seq.LimitedSequence;
import com.googlecode.aviator.runtime.type.seq.MapSequence;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Utils;

/**
 * Runtime utils
 *
 * @author dennis
 *
 */
public final class RuntimeUtils {

  private static final int CHECKPOINTS =
      Integer.parseInt(System.getProperty("aviator.execution.timeout.checkpoints", "2000"));

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
   * Wrap the function to unpacking-arguments function.
   *
   * @param fn
   * @return
   */
  public static final AviatorFunction unpackArgsFunction(final AviatorFunction fn) {
    if (fn instanceof UnpackingArgsFunction) {
      return fn;
    }
    return new UnpackingArgsFunction(fn);
  }

  public static void resetLambdaContext(AviatorFunction fn) {
    if (fn != null && fn instanceof LambdaFunction) {
      ((LambdaFunction) fn).resetContext();
    }
  }

  /**
   * Cast an object into sequence if possible, otherwise throw an exception.
   *
   * @param o
   * @return
   */
  @SuppressWarnings({"rawtypes", "unchecked"})
  public static Sequence seq(final Object o, final Map<String, Object> env) {
    Sequence seq = null;

    if (o == null) {
      return EmptySequence.INSTANCE;
    } else if (o instanceof Sequence) {
      seq = (Sequence) o;
    } else if (o instanceof CharSequence) {
      seq = new CharSeqSequence((CharSequence) o);
    } else if (o instanceof Iterable) {
      seq = new IterableSequence((Iterable) o);
    } else if (o.getClass().isArray()) {
      seq = new ArraySequence(o);
    } else if (o instanceof Map) {
      seq = new MapSequence((Map) o);
    } else {
      throw new IllegalArgumentException(o + " is not a sequence");
    }

    if (env instanceof Env) {
      int maxLoopCount =
          RuntimeUtils.getInstance(env).getOptionValue(Options.MAX_LOOP_COUNT).number;
      if (maxLoopCount > 0) {
        return new LimitedSequence<>(seq, maxLoopCount);
      }
    }

    return seq;
  }

  public static void checkExecutionTimedOut(final Map<String, Object> env) {
    if (env instanceof Env) {
      Env theEnv = (Env) env;
      long startNs = theEnv.getStartNs();
      if (startNs > 0) {
        long execTimeoutNs = getEvalTimeoutNs(env);
        if (execTimeoutNs > 0) {
          if (theEnv.incExecCheckpointsAndGet() % CHECKPOINTS == 0) {
            if (Utils.currentTimeNanos() - startNs > execTimeoutNs) {
              throw new TimeoutException("Expression execution timed out, exceeded: "
                  + getInstance(env).getOptionValue(Options.EVAL_TIMEOUT_MS).number + " ms");
            }
          }
        }
      }
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

  // Returns the eval timeout value in nanoseconds
  public static final long getEvalTimeoutNs(final Map<String, Object> env) {
    return getInstance(env).getOptionValue(Options.EVAL_TIMEOUT_MS).cachedNumber;
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
