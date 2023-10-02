package com.googlecode.aviator.runtime.function.internal;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Reflector;

/**
 * __try(try_func, catch_handlers, finally_fn, callcc)
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class TryCatchFunction extends AbstractFunction {

  private static final long serialVersionUID = 7314510329619948965L;

  private TryCatchFunction() {}

  public static final TryCatchFunction INSTANCE = new TryCatchFunction();

  @Override
  public String getName() {
    return "__try";
  }

  @SuppressWarnings("unchecked")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4) {
    AviatorFunction tryBody = (AviatorFunction) arg1;
    List<CatchHandler> catchHandlers = (List<CatchHandler>) arg2.getValue(env);
    AviatorFunction finallyBody = arg3 != AviatorNil.NIL ? (AviatorFunction) arg3 : null;
    AviatorObject result = null;
    try {
      result = tryBody.call(env);
    } catch (Throwable t) {
      boolean handle = false;
      if (catchHandlers != null) {
        for (CatchHandler handler : catchHandlers) {
          if (handler.isMatch(t.getClass())) {
            AviatorObject ret = handler.getFunc().call(env, AviatorRuntimeJavaType.valueOf(t));
            result = chooseResult(result, ret);
            handle = true;
            break;
          }
        }
      }
      if (!handle) {
        throw Reflector.sneakyThrow(t);
      }
    } finally {
      try {
        if (finallyBody != null) {
          AviatorObject ret = finallyBody.call(env);
          result = chooseResult(result, ret);
        }
      } finally {
        RuntimeUtils.resetLambdaContext(tryBody);
        RuntimeUtils.resetLambdaContext(finallyBody);
      }
    }

    if (isReturnResult(result)) {
      return result;
    }

    Object val = arg4.getValue(env);
    if ((val instanceof ReducerResult) && ((ReducerResult) val).isEmptyState()) {
      return result;
    }
    AviatorFunction continueFn = (AviatorFunction) val;
    try {
      AviatorObject contResult = continueFn.call(env);
      if ((contResult instanceof ReducerResult) && ((ReducerResult) contResult).isEmptyState()) {
        return result;
      } else {
        return contResult;
      }
    } finally {
      RuntimeUtils.resetLambdaContext(continueFn);
    }
  }

  public AviatorObject chooseResult(final AviatorObject result, final AviatorObject ret) {
    if (result instanceof ReducerResult) {
      if (ret instanceof ReducerResult && isNewState(result, ret)) {
        return ret;
      }
      return result;
    } else {
      return ret;
    }
  }

  private boolean isNewState(final AviatorObject result, final AviatorObject ret) {
    return ((ReducerResult) ret).state.compareTo(((ReducerResult) result).state) >= 0;
  }

  private boolean isReturnResult(final AviatorObject ret) {
    return ret instanceof ReducerResult && ((ReducerResult) ret).state == ReducerState.Return;
  }

}
