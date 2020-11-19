package com.googlecode.aviator.runtime.function.math;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * math.ceil(d) function
 *
 * @author dennis
 *
 */
public class MathCeilFunction extends AbstractFunction {


  private static final long serialVersionUID = 9071009020940674955L;

  private MathCeilFunction() {

  }

  public static final MathCeilFunction INSTANCE = new MathCeilFunction();

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg) {
    return AviatorDouble.valueOf(Math.ceil(FunctionUtils.getNumberValue(arg, env).doubleValue()));
  }


  @Override
  public String getName() {
    return "math.ceil";
  }

}
