package com.googlecode.aviator.runtime.op;

import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.RuntimeUtils;
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

  private static final ThreadLocal<AviatorObject[]> ONE_ARG = new ThreadLocal<AviatorObject[]>() {

    @Override
    protected AviatorObject[] initialValue() {
      return new AviatorObject[1];
    }

  };

  /**
   * Eval with arguments array.
   *
   * @param args
   * @param opType
   * @return
   */
  public static AviatorObject eval(Map<String, Object> env, AviatorObject[] args,
      OperatorType opType) {
    AviatorFunction func = RuntimeUtils.getInstance(env).getOpFunction(opType);
    AviatorObject ret = eval0(env, args, opType, func);
    if (RuntimeUtils.isTracedEval(env)) {
      trace(null, opType, ret, args);
    }
    return ret;
  }

  private static AviatorObject eval0(Map<String, Object> env, AviatorObject[] args,
      OperatorType opType, AviatorFunction func) {
    if (func == null) {
      return opType.eval(args, env);
    } else {
      switch (args.length) {
        case 1:
          return func.call(env, args[0]);
        case 2:
          return func.call(env, args[0], args[1]);
        case 3:
          return func.call(env, args[0], args[1], args[2]);
      }
      throw new ExpressionRuntimeException("Too many arguments.");
    }
  }

  /**
   * Eval with unary operator
   *
   * @param arg
   * @param env
   * @param opType
   * @return
   */
  public static AviatorObject eval(AviatorObject arg, Map<String, Object> env,
      OperatorType opType) {
    AviatorFunction func = RuntimeUtils.getInstance(env).getOpFunction(opType);
    AviatorObject ret = eval0(arg, env, opType, func);
    if (RuntimeUtils.isTracedEval(env)) {
      trace(env, opType, ret, arg);
    }
    return ret;
  }

  private static AviatorObject eval0(AviatorObject arg, Map<String, Object> env,
      OperatorType opType, AviatorFunction func) {
    if (func == null) {
      AviatorObject[] args = ONE_ARG.get();
      args[0] = arg;
      return opType.eval(args, env);
    } else {
      return func.call(env, arg);
    }
  }

  /**
   * Just like {@link #eval(AviatorObject, AviatorObject, Map, OperatorType)}, but with difference
   * arguments order.
   *
   * @param left
   * @param env
   * @param right
   * @param opType
   * @return
   */
  public static AviatorObject eval(AviatorObject left, Map<String, Object> env, AviatorObject right,
      OperatorType opType) {
    return eval(left, right, env, opType);
  }

  /**
   * Eval with binary operator
   *
   * @param left
   * @param right
   * @param env
   * @param opType
   * @return
   */
  public static AviatorObject eval(AviatorObject left, AviatorObject right, Map<String, Object> env,
      OperatorType opType) {

    AviatorFunction func = RuntimeUtils.getInstance(env).getOpFunction(opType);
    AviatorObject ret = eval0(left, right, env, opType, func);
    if (RuntimeUtils.isTracedEval(env)) {
      trace(env, opType, ret, left, right);
    }
    return ret;
  }

  private static AviatorObject eval0(AviatorObject left, AviatorObject right,
      Map<String, Object> env, OperatorType opType, AviatorFunction func) {
    if (func == null) {
      AviatorObject[] args = TWO_ARRGS.get();
      args[0] = left;
      args[1] = right;
      return opType.eval(args, env);
    } else {
      return func.call(env, left, right);
    }
  }

  public static final boolean hasRuntimeContext(Map<String, Object> env, OperatorType opType) {
    return RuntimeUtils.getInstance(env).getOpsMap().containsKey(opType)
        || RuntimeUtils.isTracedEval(env);
  }

  private static final String WHITE_SPACE = " ";
  private static final String TRACE_PREFIX = "         ";

  private static void trace(Map<String, Object> env, OperatorType opType, AviatorObject result,
      AviatorObject... args) {

    switch (args.length) {
      case 1:
        RuntimeUtils.printTrace(env,
            TRACE_PREFIX + opType.token + args[0].desc(env) + " => " + result.desc(env));
        break;
      case 2:
        RuntimeUtils.printTrace(env, TRACE_PREFIX + args[0].desc(env) + WHITE_SPACE + opType.token
            + WHITE_SPACE + args[1].desc(env) + " => " + result.desc(env));
        break;
      case 3:
        RuntimeUtils.printTrace(env,
            TRACE_PREFIX + args[0].desc(env) + WHITE_SPACE + "?" + WHITE_SPACE + args[0].desc(env)
                + WHITE_SPACE + ":" + WHITE_SPACE + args[1].desc(env) + " => " + result.desc(env));
        break;
    }
  }
}
