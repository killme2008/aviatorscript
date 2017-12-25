package com.googlecode.aviator.runtime.op;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Operation runtime
 *
 * @author dennis
 *
 */
public class OperationRuntime {

  private static final ThreadLocal<AviatorObject[]> TWO_ARRGS = new ThreadLocal<AviatorObject[]>() {

    @Override
    protected AviatorObject[] initialValue() {
      return new AviatorObject[2];
    }

  };

  public static AviatorObject eval(AviatorObject[] args, OperatorType opType) {
    AviatorFunction func = AviatorEvaluator.getOpFunction(opType);
    if (func == null) {
      return opType.eval(args, null);
    } else {
      switch (args.length) {
        case 1:
          return func.call(null, args[0]);
        case 2:
          return func.call(null, args[0], args[1]);
        case 3:
          return func.call(null, args[0], args[1], args[2]);
      }
      throw new ExpressionRuntimeException("eval with too many arguments.");
    }
  }

  public static AviatorObject eval(AviatorObject left, AviatorObject right, Map<String, Object> env,
      OperatorType opType) {

    AviatorFunction func = AviatorEvaluator.getOpFunction(opType);
    if (func == null) {
      AviatorObject[] args = TWO_ARRGS.get();
      args[0] = left;
      args[1] = right;
      return opType.eval(args, env);
    } else {
      return func.call(env, left, right);
    }
  }

  public static final boolean hasCustomOperatorFunctions() {
    return !AviatorEvaluator.OPS_MAP.isEmpty();
  }
}
