package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to boolean, return false when nil or false, otherwise returns true.
 *
 * @author dennis
 * @Date 2018-5-18
 * @since 4.0.0
 *
 */
public class BooleanFunction extends AbstractFunction {


  private static final long serialVersionUID = -2549798338853017229L;


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    switch (arg1.getAviatorType()) {
      case Boolean:
        return arg1;
      default:
        if (arg1.getValue(env) == null) {
          return AviatorBoolean.FALSE;
        } else {
          return AviatorBoolean.TRUE;
        }
    }
  }


  @Override
  public String getName() {
    return "boolean";
  }

}
