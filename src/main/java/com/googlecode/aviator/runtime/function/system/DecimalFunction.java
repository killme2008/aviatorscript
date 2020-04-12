package com.googlecode.aviator.runtime.function.system;

import java.math.BigDecimal;
import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorDecimal;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to decimal.
 *
 * @author dennis
 * @Date 2011-5-18
 * @since 1.1.1
 *
 */
public class DecimalFunction extends AbstractFunction {


  private static final long serialVersionUID = 820173052464302490L;


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    switch (arg1.getAviatorType()) {
      case Boolean:
        return AviatorDecimal.valueOf(arg1.booleanValue(env) ? BigDecimal.ONE : BigDecimal.ZERO);
      case JavaType:
        Object obj = arg1.getValue(env);
        if (obj instanceof Number) {
          return AviatorDecimal
              .valueOf(new BigDecimal(String.valueOf(obj), RuntimeUtils.getMathContext(env)));
        } else if (obj instanceof String) {
          return AviatorDecimal
              .valueOf(new BigDecimal((String) obj, RuntimeUtils.getMathContext(env)));
        } else if (obj instanceof Character) {
          return AviatorDecimal
              .valueOf(new BigDecimal(String.valueOf(obj), RuntimeUtils.getMathContext(env)));
        } else {
          throw new ClassCastException(
              "Could not cast " + obj.getClass().getName() + " to decimal");
        }
      case String:
        return AviatorDecimal
            .valueOf(new BigDecimal((String) arg1.getValue(env), RuntimeUtils.getMathContext(env)));
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return AviatorDecimal.valueOf(((AviatorNumber) arg1).toDecimal(env));
      default:
        throw new ClassCastException("Could not cast " + arg1 + " to decimal");
    }
  }


  @Override
  public String getName() {
    return "decimal";
  }

}
