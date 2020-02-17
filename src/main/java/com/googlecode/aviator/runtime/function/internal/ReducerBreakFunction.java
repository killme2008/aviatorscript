package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

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
    return AviatorRuntimeJavaType.wrap(ReducerResult.withBreak(AviatorNil.NIL));
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return ReducerResult.withBreak(AviatorNil.NIL);
  }
}
