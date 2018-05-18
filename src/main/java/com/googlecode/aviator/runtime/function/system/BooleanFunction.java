package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to double,double(a) eg.
 *
 * @author dennis
 * @Date 2018-5-18
 * @since 4.0.0
 *
 */
public class BooleanFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
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
