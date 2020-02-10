package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * __if_return function
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class IfReturnFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "__if_return";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    System.out.println("Got " + arg1 + " " + arg1.getClass());
    if (arg1 instanceof ReducerResult && ((ReducerResult) arg1).state != ReducerState.Cont) {
      return AviatorRuntimeJavaType.wrap(arg1);
    } else {
      AviatorFunction otherClausesFn = FunctionUtils.getFunction(arg2, env, 0);
      AviatorObject result = otherClausesFn.call(env);
      if (result == AviatorEvaluatorInstance.REDUCER_EMPTY) {
        System.out.println("Got " + result);
        return AviatorRuntimeJavaType.wrap(arg1);
      }
      System.out.println("Got " + result);
      return result;
    }
  }
}
