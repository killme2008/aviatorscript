package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Env;

/**
 * undef(x) to forgot a variable that is defined in current scope.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class UndefFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "undef";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    if (arg1.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException(
          "Invalid argument type for undef: " + arg1.getAviatorType());
    }
    return FunctionUtils.wrapReturn(((Env) env).forgot(((AviatorJavaType) arg1).getName()));
  }

}
