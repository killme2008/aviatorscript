package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to long
 *
 * @author dennis
 * @Date 2011-5-18
 * @since 1.1.1
 *
 */
public class LongFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    switch (arg1.getAviatorType()) {
      case Boolean:
        return AviatorLong.valueOf(arg1.booleanValue(env) ? 1 : 0);
      case JavaType:
        Object obj = arg1.getValue(env);
        if (obj instanceof Number) {
          return AviatorLong.valueOf(((Number) obj).longValue());
        } else if (obj instanceof String) {
          return AviatorLong.valueOf(Long.valueOf((String) obj));
        } else if (obj instanceof Character) {
          return AviatorLong.valueOf(Long.valueOf(String.valueOf(obj)));
        } else {
          throw new ClassCastException("Could not cast " + obj.getClass().getName() + " to long");
        }
      case String:
        return AviatorLong.valueOf(Long.valueOf((String) arg1.getValue(env)));
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return AviatorLong.valueOf(((Number) arg1.getValue(env)).longValue());
      default:
        throw new ClassCastException("Could not cast " + arg1 + " to long");
    }
  }


  @Override
  public String getName() {
    return "long";
  }

}
