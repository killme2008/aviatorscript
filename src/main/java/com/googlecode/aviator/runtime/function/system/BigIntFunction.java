package com.googlecode.aviator.runtime.function.system;

import java.math.BigInteger;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBigInt;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to bigint
 *
 * @author dennis
 * @Date 2011-5-18
 * @since 1.1.1
 *
 */
public class BigIntFunction extends AbstractFunction {


  private static final long serialVersionUID = 820173052464302490L;


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    switch (arg1.getAviatorType()) {
      case Boolean:
        return AviatorBigInt.valueOf(arg1.booleanValue(env) ? BigInteger.ONE : BigInteger.ZERO);
      case JavaType:
        Object obj = arg1.getValue(env);
        if (obj instanceof Number) {
          return AviatorBigInt.valueOf(new BigInteger(String.valueOf(obj)));
        } else if (obj instanceof String) {
          return AviatorBigInt.valueOf(new BigInteger((String) obj));
        } else if (obj instanceof Character) {
          return AviatorBigInt.valueOf(new BigInteger(String.valueOf(obj)));
        } else {
          throw new ClassCastException(
              "Could not cast " + (obj != null ? obj.getClass().getName() : "null")
                  + " to bigint, AviatorObject is " + arg1);
        }
      case String:
        return AviatorBigInt.valueOf(new BigInteger((String) arg1.getValue(env)));
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return AviatorBigInt.valueOf(((AviatorNumber) arg1).toBigInt());
      default:
        throw new ClassCastException("Could not cast " + arg1 + " to bigint");
    }
  }


  @Override
  public String getName() {
    return "bigint";
  }

}
