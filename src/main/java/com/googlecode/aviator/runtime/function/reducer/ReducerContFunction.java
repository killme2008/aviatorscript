package com.googlecode.aviator.runtime.function.reducer;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Internal reducer-continue function for 'for-loop' structure.
 *
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ReducerContFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "__reducer_cont";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return new ReducerResult(ReducerState.Cont);
  }

}
