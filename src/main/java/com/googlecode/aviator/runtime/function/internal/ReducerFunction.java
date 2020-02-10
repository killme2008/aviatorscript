package com.googlecode.aviator.runtime.function.internal;

import java.lang.reflect.Array;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * Internal reducer function for 'for-loop' structure.
 *
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ReducerFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "__reducer";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {

    Object coll = arg1.getValue(env);
    AviatorFunction iteratorFn = FunctionUtils.getFunction(arg2, env, 2);
    AviatorFunction remainingFn = FunctionUtils.getFunction(arg3, env, 0);
    if (iteratorFn == null) {
      throw new FunctionNotFoundException(
          "There is no function named " + ((AviatorJavaType) arg2).getName());
    }
    if (remainingFn == null) {
      throw new FunctionNotFoundException("remainingFn not found, Shoud not happen");
    }
    if (coll == null) {
      throw new NullPointerException("null seq");
    }
    AviatorObject result = null;

    Class<?> clazz = coll.getClass();

    if (Iterable.class.isAssignableFrom(clazz)) {
      for (Object obj : (Iterable<?>) coll) {
        result = iteratorFn.call(env, AviatorRuntimeJavaType.valueOf(obj));
        if (!(result instanceof ReducerResult)) {
          continue;
        }

        ReducerResult intermediateResult = (ReducerResult) result;
        if (intermediateResult.state == ReducerState.Cont) {
          continue;
        } else if (intermediateResult.state == ReducerState.Break) {
          break;
        } else {
          return AviatorRuntimeJavaType.wrap(intermediateResult);
        }
      }
    } else if (Map.class.isAssignableFrom(clazz)) {
      for (Object obj : ((Map<?, ?>) coll).entrySet()) {
        result = iteratorFn.call(env, AviatorRuntimeJavaType.valueOf(obj));
        if (result.isNull(env) || !(result instanceof ReducerResult)) {
          continue;
        }

        ReducerResult intermediateResult = (ReducerResult) result;
        if (intermediateResult.state == ReducerState.Cont) {
          continue;
        } else if (intermediateResult.state == ReducerState.Break) {
          break;
        } else {
          return AviatorRuntimeJavaType.wrap(intermediateResult);
        }
      }
    } else if (clazz.isArray()) {
      int length = Array.getLength(coll);
      for (int i = 0; i < length; i++) {
        Object obj = Array.get(coll, i);
        result = iteratorFn.call(env, AviatorRuntimeJavaType.valueOf(obj));
        if (result.isNull(env) || !(result instanceof ReducerResult)) {
          continue;
        }

        ReducerResult intermediateResult = (ReducerResult) result;
        if (intermediateResult.state == ReducerState.Cont) {
          continue;
        } else if (intermediateResult.state == ReducerState.Break) {
          break;
        } else {
          return AviatorRuntimeJavaType.wrap(intermediateResult);
        }
      }
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a seq");
    }

    AviatorObject otherResult = remainingFn.call(env);
    if (otherResult == AviatorEvaluatorInstance.REDUCER_EMPTY) {
      return AviatorRuntimeJavaType.wrap(result);
    } else {
      return otherResult;
    }
  }

}
