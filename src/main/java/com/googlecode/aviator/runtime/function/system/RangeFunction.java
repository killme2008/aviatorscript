package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.Range;

/**
 * A function to create a range in [start, end] with step.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class RangeFunction extends AbstractFunction {


  private static final long serialVersionUID = -8074216928792719019L;

  @Override
  public String getName() {
    return "range";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    return new Range(AviatorNumber.valueOf(arg1.getValue(env)),
        AviatorNumber.valueOf(arg2.getValue(env)), AviatorLong.valueOf(1));
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {
    return new Range((AviatorNumber) arg1, (AviatorNumber) arg2, (AviatorNumber) arg3);
  }

}
