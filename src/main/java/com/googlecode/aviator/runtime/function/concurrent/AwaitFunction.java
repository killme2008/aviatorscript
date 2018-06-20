package com.googlecode.aviator.runtime.function.concurrent;

import java.util.Map;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.Future;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Await a future finished.
 *
 * @author dennis
 *
 */
public class AwaitFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "await";
  }

  @SuppressWarnings("unchecked")
  @Override
  public AviatorObject call(final Map<String, Object> env, AviatorObject arg1) {
    final Object future = arg1.getValue(env);
    if (!(future instanceof Future<?>)) {
      throw new ClassCastException("Could not cast " + future.getClass().getName() + " to future.");
    }

    try {
      return ((Future<AviatorObject>) future).get();
    } catch (ExecutionException e) {
      throw new ExpressionRuntimeException("Fail to await on future", e);
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw new ExpressionRuntimeException("Await on future was interrupted", e);
    }
  }
}
