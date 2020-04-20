package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.exception.StandardError;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Reflector;

/**
 * __throw(e) to throw an exception.
 *
 * @author boyan(boyan@antfin.com)
 *
 */
public class ThrowFunction extends AbstractFunction {

  private static final long serialVersionUID = -8464670257920503718L;

  @Override
  public String getName() {
    return "__throw";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    Object val = arg1.getValue(env);
    if (val instanceof Throwable) {
      throw Reflector.sneakyThrow((Throwable) val);
    } else {
      throw Reflector.sneakyThrow(new StandardError(val == null ? null : val.toString()));
    }
  }

}
