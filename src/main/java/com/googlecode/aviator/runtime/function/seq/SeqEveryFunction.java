package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * Returns true if fun.call(x) is logical true for every x in sequence, else false.
 */
public class SeqEveryFunction extends AbstractFunction {


  private static final long serialVersionUID = 3336351857807826640L;


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    Object first = arg1.getValue(env);
    AviatorFunction fun = FunctionUtils.getFunction(arg2, env, 1);
    if (fun == null) {
      throw new FunctionNotFoundException(
          "There is no function named " + ((AviatorJavaType) arg2).getName());
    }
    if (first == null) {
      return AviatorBoolean.TRUE;
    }

    for (Object obj : RuntimeUtils.seq(first, env)) {
      if (!fun.call(env, AviatorRuntimeJavaType.valueOf(obj)).booleanValue(env)) {
        return AviatorBoolean.FALSE;
      }
    }

    return AviatorBoolean.TRUE;
  }


  @Override
  public String getName() {
    return "seq.every";
  }
}
