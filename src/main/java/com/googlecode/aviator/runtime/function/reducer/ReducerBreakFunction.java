package com.googlecode.aviator.runtime.function.reducer;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Internal reducer-break function for 'for-loop' structure.
 *
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ReducerBreakFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "__reducer_break";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env) {
    ReducerResult reducerResult = new ReducerResult(ReducerState.Break);
    return reducerResult;
  }
}
