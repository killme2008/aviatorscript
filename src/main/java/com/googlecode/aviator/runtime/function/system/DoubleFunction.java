package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to double,double(a) eg.
 *
 * @author dennis
 * @Date 2011-6-23
 * @since 1.1.1
 *
 */
public class DoubleFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    switch (arg1.getAviatorType()) {
      case Boolean:
        return new AviatorDouble(arg1.booleanValue(env) ? 1 : 0);
      case JavaType:
        Object obj = arg1.getValue(env);
        if (obj instanceof Number) {
          return new AviatorDouble(((Number) obj).doubleValue());
        } else if (obj instanceof String) {
          return new AviatorDouble(Double.parseDouble((String) obj));
        } else if (obj instanceof Character) {
          return new AviatorDouble(Double.parseDouble(String.valueOf(obj)));
        } else {
          throw new ClassCastException("Could not cast " + obj.getClass().getName() + " to double");
        }
      case String:
        return new AviatorDouble(Double.parseDouble((String) arg1.getValue(env)));
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return new AviatorDouble(((Number) arg1.getValue(env)).doubleValue());
      default:
        throw new ClassCastException("Could not cast " + arg1 + " to double");
    }
  }


  @Override
  public String getName() {
    return "double";
  }

}
