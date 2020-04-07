package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;

/**
 * is_def(x) returns true when variable x is defined in current scope or parent scopes.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class IsDefFunction extends AbstractFunction {


  private static final long serialVersionUID = 8641929538658275527L;

  @Override
  public String getName() {
    return "is_def";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    if (arg1.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException(
          "Invalid argument type for is_def: " + arg1.getAviatorType());
    }
    return AviatorBoolean.valueOf(env.containsKey(((AviatorJavaType) arg1).getName()));
  }

}
