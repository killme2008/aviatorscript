package com.googlecode.aviator;

import java.math.MathContext;


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
  PUT_CAPTURING_GROUPS_INTO_ENV;


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
        return val instanceof Boolean;
      case OPTIMIZE_LEVEL:
        return val instanceof Integer && (((Integer) val).intValue() == AviatorEvaluator.EVAL
            || ((Integer) val).intValue() == AviatorEvaluator.COMPILE);
      case MATH_CONTEXT:
        return val instanceof MathContext;
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
    }
    return null;
  }
}
