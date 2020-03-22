package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * Returns false if fun.call(x) is logical true for any x in sequence, else true.
 */
public class SeqNotAnyFunction extends AbstractFunction {

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
      return AviatorNil.NIL;
    }

    for (Object obj : RuntimeUtils.seq(first)) {
      if (fun.call(env, AviatorRuntimeJavaType.valueOf(obj)).booleanValue(env)) {
        return AviatorBoolean.FALSE;
      }
    }
    return AviatorBoolean.TRUE;
  }


  @Override
  public String getName() {
    return "seq.not_any";
  }
}
