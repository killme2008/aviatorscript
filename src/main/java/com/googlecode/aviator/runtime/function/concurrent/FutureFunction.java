package com.googlecode.aviator.runtime.function.concurrent;

import java.util.Map;
import java.util.concurrent.Callable;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * Future function, that will starts a thread to run the function argument.
 *
 * @author dennis
 *
 */
public class FutureFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "future";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, AviatorObject arg1) {
    final Object func = arg1.getValue(env);
    if (!(func instanceof AviatorFunction)) {
      throw new ClassCastException("Could not cast " + func.getClass().getName() + " to function");
    }

    return new AviatorRuntimeJavaType(
        RuntimeUtils.getFutureExecutor(env).submit(new Callable<AviatorObject>() {

          @Override
          public AviatorObject call() {
            return ((AviatorFunction) func).call(env);

          }
        }));
  }
}
