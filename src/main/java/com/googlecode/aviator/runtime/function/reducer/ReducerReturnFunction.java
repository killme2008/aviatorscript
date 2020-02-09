package com.googlecode.aviator.runtime.function.reducer;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * Internal reducer-return function for 'for-loop' structure.
 *
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ReducerReturnFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "__reducer_return";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return new AviatorRuntimeJavaType(new ReducerResult(ReducerState.Return, arg1));
  }

}
