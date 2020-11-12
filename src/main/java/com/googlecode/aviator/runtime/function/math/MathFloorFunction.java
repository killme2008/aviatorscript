package com.googlecode.aviator.runtime.function.math;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * math.floor(d) function
 *
 * @author dennis
 *
 */
public class MathFloorFunction extends AbstractFunction {


  private static final long serialVersionUID = 9071009020940674955L;

  private MathFloorFunction() {

  }

  public static final MathFloorFunction INSTANCE = new MathFloorFunction();

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg) {
    return AviatorLong.valueOf(Math.floor(FunctionUtils.getNumberValue(arg, env).doubleValue()));
  }


  @Override
  public String getName() {
    return "math.floor";
  }

}
