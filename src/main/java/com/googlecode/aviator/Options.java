package com.googlecode.aviator;

import java.math.MathContext;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.atomic.AtomicInteger;


/**
 * Aviator Evaluator Configuration options.
 *
 * @author dennis
 *
 */
public enum Options {
  /**
   * Always use double as BigDecimal, default is false.
   *
   * @deprecated This is a name typo option, please use
   *             {@link #ALWAYS_USE_DECIMAL_AS_FLOATING_POINT_NUMBER} instead.
   * @since 2.3.4
   */
  ALWAYS_USE_DOUBLE_AS_DECIMAL,

  /**
   * Optimize level, default is {@link AviatorEvaluator#EVAL}
   *
   * @see AviatorEvaluator#EVAL
   * @see AviatorEvaluator#COMPILE
   */
  OPTIMIZE_LEVEL,

  /**
   * Math context for decimal, default is {@link MathContext.DECIMAL128}
   *
   * @see MathContext
   */
  MATH_CONTEXT,

  /**
   * Whether to trace code generation,default is false.
   *
   * @deprecated
   */
  TRACE,
  /**
   * Always parsing floating-point number into BigDecial, default is false.It replaces
   * {@link #ALWAYS_USE_DOUBLE_AS_DECIMAL} option.
   */
  ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL,
  /**
   * Whether to trace expression evaluating procedure, default is false.
   */
  TRACE_EVAL,
  /**
   * Whether to put capturing groups into passed-in env map when regular-expression pattern matches,
   * default is true.If you don't want the groups, you can turn it off to get better performance in
   * regular-expression pattern matching.
   */
  PUT_CAPTURING_GROUPS_INTO_ENV,

  /**
   * Enable property access syntax sugar, use common-beantuils to access property such as "a.b.c"
   * etc. Default value is true, enable this behaviour.
   */
  ENABLE_PROPERTY_SYNTAX_SUGAR,

  /**
   * When enable property access syntax sugar, returns nil if the property value is not found or
   * throws exception.Default value is false,disabled this behaviour.
   */
  NIL_WHEN_PROPERTY_NOT_FOUND,
  /**
   * Future function executor,default is a {@link Executors#newCachedThreadPool()}.
   */
  FUTURE_EXECUTOR;


  /**
   * Default executor for future function.
   */
  private static final ExecutorService DEFAULT_FUTURE_THREAD_POOL =
      Executors.newCachedThreadPool(new ThreadFactory() {
        AtomicInteger counter = new AtomicInteger(0);

        @Override
        public Thread newThread(Runnable r) {
          Thread t = new Thread(r);
          t.setDaemon(true);
          t.setName("AviatorFutureExecutor-" + counter.incrementAndGet());
          return t;
        }
      });
  private static final Boolean TRACE_DEFAULT_VAL =
      Boolean.valueOf(System.getProperty("aviator.asm.trace", "false"));
  private static final Boolean TRACE_EVAL_DEFAULT_VAL =
      Boolean.valueOf(System.getProperty("aviator.trace_eval", "false"));


  public boolean isValidValue(Object val) {
    switch (this) {
      case ALWAYS_USE_DOUBLE_AS_DECIMAL:
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case TRACE:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
        return val instanceof Boolean;
      case OPTIMIZE_LEVEL:
        return val instanceof Integer && (((Integer) val).intValue() == AviatorEvaluator.EVAL
            || ((Integer) val).intValue() == AviatorEvaluator.COMPILE);
      case MATH_CONTEXT:
        return val instanceof MathContext;
      case FUTURE_EXECUTOR:
        return val instanceof ExecutorService;
    }
    return false;
  }


  /**
   * Returns the default value of option.
   *
   * @return
   */
  public Object getDefaultValue() {
    switch (this) {
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_USE_DOUBLE_AS_DECIMAL:
        return false;
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
        return true;
      case NIL_WHEN_PROPERTY_NOT_FOUND:
        return false;
      case OPTIMIZE_LEVEL:
        return AviatorEvaluator.EVAL;
      case MATH_CONTEXT:
        return MathContext.DECIMAL128;
      case TRACE_EVAL:
        return TRACE_EVAL_DEFAULT_VAL;
      case TRACE:
        return TRACE_DEFAULT_VAL;
      case PUT_CAPTURING_GROUPS_INTO_ENV:
        return true;
      case FUTURE_EXECUTOR:
        return DEFAULT_FUTURE_THREAD_POOL;
    }
    return null;
  }
}
