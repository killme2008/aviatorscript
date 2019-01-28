package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * identity function return the argument itself.
 * 
 * @author dennis
 *
 */
public class IdentityFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "identity";
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    return arg1;
  }
}
