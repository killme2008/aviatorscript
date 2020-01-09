package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * A compare function.
 *
 * @author killme2008(killme2008@gmail.com)
 *
 */
public class CompareFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "cmp";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    return AviatorLong.valueOf(arg1.compare(arg2, env));
  }

}
