package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.Range;
import com.googlecode.aviator.utils.Constants;

/**
 * Internal reducer-callcc function for 'for-loop' structure.
 *
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ReducerFunction extends AbstractFunction {


  private static final long serialVersionUID = -6117602709327741955L;

  @Override
  public String getName() {
    return "__reducer_callcc";
  }

  @Override
  public final AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {

    Object coll = arg1.getValue(env);
    AviatorFunction iteratorFn = (AviatorFunction) arg2;

    int maxLoopCount = RuntimeUtils.getInstance(env).getOptionValue(Options.MAX_LOOP_COUNT).number;
    AviatorObject result = null;
    long c = 0;

    if (coll != Range.LOOP) {
      // for..in loop
      for (Object obj : RuntimeUtils.seq(coll, env)) {
        result = iteratorFn.call(env, AviatorRuntimeJavaType.valueOf(obj));
        if (!(result instanceof ReducerResult)) {
          continue;
        }

        boolean breakOut = false;
        ReducerResult midResult = (ReducerResult) result;
        result = midResult.obj;
        switch (midResult.state) {
          case Break:
            breakOut = true;
            break;
          case Return:
            return midResult;
          default:
            break;
        }
        if (breakOut) {
          break;
        }
      }
    } else {
      // while statement
      while (true) {
        if (maxLoopCount > 0 && ++c > maxLoopCount) {
          throw new ExpressionRuntimeException("Overflow max loop count: " + maxLoopCount);
        }
        result = iteratorFn.call(env);
        if (!(result instanceof ReducerResult)) {
          continue;
        }
        boolean breakOut = false;
        ReducerResult midResult = (ReducerResult) result;
        result = midResult.obj;
        switch (midResult.state) {
          case Break:
            breakOut = true;
            break;
          case Return:
            return midResult;
          default:
            break;
        }
        if (breakOut) {
          break;
        }
      }
    }

    Object contObj = arg3.getValue(env);
    if (contObj == Constants.REDUCER_EMPTY) {
      return result;
    }

    AviatorObject contResult = ((AviatorFunction) contObj).call(env);
    if (contResult == Constants.REDUCER_EMPTY) {
      // empty continuation, return current result.
      return result;
    } else {
      return contResult;
    }
  }

}
