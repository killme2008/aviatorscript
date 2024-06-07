package com.googlecode.aviator.runtime.op;

import java.util.Map;
import com.googlecode.aviator.exception.IllegalArityException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.Variable;
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
  public static AviatorObject eval(final Map<String, Object> env, final AviatorObject[] args,
      final OperatorType opType) {
    AviatorFunction func = RuntimeUtils.getInstance(env).getOpFunction(opType);
    AviatorObject ret = eval0(env, args, opType, func);
    if (RuntimeUtils.isTracedEval(env)) {
      trace(env, opType, ret, args);
    }
    return ret;
  }

  private static AviatorObject eval0(final Map<String, Object> env, final AviatorObject[] args,
      final OperatorType opType, final AviatorFunction func) {
    if (func == null) {
      return opType.eval(args, env);
    } else {
      return evalOpFunction(env, args, opType, func);
    }
  }

  public static AviatorObject evalOpFunction(final Map<String, Object> env,
      final AviatorObject[] args, final OperatorType opType, final AviatorFunction func) {
    switch (opType.getArity()) {
      case 1:
        return func.call(env, args[0]);
      case 2:
        return func.call(env, args[0], args[1]);
      case 3:
        return func.call(env, args[0], args[1], args[2]);
    }
    throw new IllegalArityException("Too many arguments.");
  }

  /**
   * Eval with unary operator
   *
   * @param arg
   * @param env
   * @param opType
   * @return
   */
  public static AviatorObject eval(final AviatorObject arg, final Map<String, Object> env,
      final OperatorType opType) {
    AviatorFunction func = RuntimeUtils.getInstance(env).getOpFunction(opType);
    AviatorObject ret = eval0(arg, env, opType, func);
    if (RuntimeUtils.isTracedEval(env)) {
      trace(env, opType, ret, arg);
    }
    return ret;
  }

  private static AviatorObject eval0(final AviatorObject arg, final Map<String, Object> env,
      final OperatorType opType, final AviatorFunction func) {
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
  public static AviatorObject eval(final AviatorObject left, final Map<String, Object> env,
      final AviatorObject right, final OperatorType opType) {
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
  public static AviatorObject eval(final AviatorObject left, final AviatorObject right,
      final Map<String, Object> env, final OperatorType opType) {
    AviatorFunction func = RuntimeUtils.getInstance(env).getOpFunction(opType);
    AviatorObject ret = eval0(left, right, env, opType, func);
    if (RuntimeUtils.isTracedEval(env)) {
      trace(env, opType, ret, left, right);
    }
    return ret;
  }

  private static AviatorObject eval0(final AviatorObject left, final AviatorObject right,
      final Map<String, Object> env, final OperatorType opType, final AviatorFunction func) {
    if (func == null) {
      AviatorObject[] args = TWO_ARRGS.get();
      args[0] = left;
      args[1] = right;
      return opType.eval(args, env);
    } else {
      return func.call(env, left, right);
    }
  }

  public static final boolean hasRuntimeContext(final Map<String, Object> env,
      final OperatorType opType) {
    return containsOpFunction(env, opType) || RuntimeUtils.isTracedEval(env);
  }

  public static boolean containsOpFunction(final Map<String, Object> env,
      final OperatorType opType) {
    return RuntimeUtils.getInstance(env).getOpsMap().containsKey(opType);
  }

  private static final String WHITE_SPACE = " ";
  private static final String TRACE_PREFIX = "         ";

  private static String desc(final AviatorObject arg, final Map<String, Object> env) {
    if (arg != null) {
      return arg.desc(env);
    } else {
      return Variable.NIL.getLexeme();
    }
  }

  private static void trace(final Map<String, Object> env, final OperatorType opType,
      final AviatorObject result, final AviatorObject... args) {

    StringBuilder argsDec = new StringBuilder();
    argsDec.append(desc(args[0], env));
    for (int i = 1; i < args.length; i++) {
      if (args[i] != null) {
        argsDec.append(WHITE_SPACE).append(opType.token).append(WHITE_SPACE)
            .append(desc(args[i], env));
      }
    }

    RuntimeUtils.printlnTrace(env, TRACE_PREFIX + argsDec + " => " + desc(result, env));
  }
}
