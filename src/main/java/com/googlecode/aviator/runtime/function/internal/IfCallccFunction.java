package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Constants;

/**
 * __if_callcc function
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class IfCallccFunction extends AbstractFunction {

  private static final long serialVersionUID = 3511688119189694245L;

  private IfCallccFunction() {

  }

  public static final IfCallccFunction INSTANCE = new IfCallccFunction();

  @Override
  public String getName() {
    return "__if_callcc";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    if (arg1 instanceof ReducerResult) {
      return arg1;
    } else {
      final Object nextClauseVal = arg2.getValue(env);
      if ((nextClauseVal instanceof ReducerResult)
          && ((ReducerResult) nextClauseVal).isEmptyState()) {
        return arg1;
      }

      AviatorFunction otherClausesFn = (AviatorFunction) nextClauseVal;
      try {
        AviatorObject result = otherClausesFn.call(env);
        // No remaining statements, return the if statement result.
        if ((result instanceof ReducerResult) && ((ReducerResult) result).isEmptyState()) {
          return arg1;
        }
        return result;
      } finally {
        RuntimeUtils.resetLambdaContext(otherClausesFn);
      }
    }
  }
}
