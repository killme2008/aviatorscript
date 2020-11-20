package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Internal reducer-continue function for 'for-loop' structure.
 *
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ReducerContFunction extends AbstractFunction {


  private ReducerContFunction() {

  }

  public static final ReducerContFunction INSTANCE = new ReducerContFunction();

  private static final long serialVersionUID = 7517333105872722540L;

  @Override
  public String getName() {
    return "__reducer_cont";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return ReducerResult.withCont(AviatorNil.NIL);
  }

}
