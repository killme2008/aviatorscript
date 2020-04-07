package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * tuple(x,y,z, ...) function to return an object array.
 *
 * @author dennis
 * @since 4.0.0
 *
 */
public class TupleFunction extends AbstractVariadicFunction {


  private static final long serialVersionUID = -7377110880312266008L;

  @Override
  public String getName() {
    return "tuple";
  }

  @Override
  public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {
    Object[] tuple = new Object[args.length];
    for (int i = 0; i < args.length; i++) {
      tuple[i] = args[i].getValue(env);
    }
    return AviatorRuntimeJavaType.valueOf(tuple);
  }

}
