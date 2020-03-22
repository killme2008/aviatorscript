package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
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

  @Override
  public String getName() {
    return "__reducer_callcc";
  }

  @Override
  public final AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {

    Object coll = arg1.getValue(env);
    AviatorFunction iteratorFn = FunctionUtils.getFunction(arg2, env, 2);
    AviatorFunction continuationFn = FunctionUtils.getFunction(arg3, env, 0);
    if (iteratorFn == null) {
      throw new FunctionNotFoundException(
          "There is no function named " + ((AviatorJavaType) arg2).getName());
    }
    if (continuationFn == null) {
      throw new FunctionNotFoundException("remainingFn not found, Shoud not happen");
    }
    if (coll == null) {
      throw new NullPointerException("null seq");
    }



    AviatorObject result = null;

    if (coll == Range.LOOP) {
      // while statement
      while (true) {
        result = iteratorFn.call(env);
        if (!(result instanceof ReducerResult)) {
          continue;
        }
        ReducerResult midResult = (ReducerResult) result;
        if (midResult.state == ReducerState.Cont) {
          result = midResult.obj;
          continue;
        } else if (midResult.state == ReducerState.Break) {
          result = midResult.obj;
          break;
        } else {
          return midResult;
        }
      }
    } else {
      // for..in loop
      for (Object obj : RuntimeUtils.seq(coll)) {
        result = iteratorFn.call(env, AviatorRuntimeJavaType.valueOf(obj));
        if (!(result instanceof ReducerResult)) {
          continue;
        }

        ReducerResult midResult = (ReducerResult) result;
        if (midResult.state == ReducerState.Cont) {
          result = midResult.obj;
          continue;
        } else if (midResult.state == ReducerState.Break) {
          result = midResult.obj;
          break;
        } else {
          return midResult;
        }
      }
    }


    AviatorObject contResult = continuationFn.call(env);
    if (contResult == Constants.REDUCER_EMPTY) {
      // empty continuation, return current result.
      return result;
    } else {
      return contResult;
    }
  }

}
