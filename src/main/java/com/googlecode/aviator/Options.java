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
   * When true, always parsing floating-point number into BigDecial, default is false.It replaces
   * {@link #ALWAYS_USE_DOUBLE_AS_DECIMAL} option.
   */
  ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL,
  /**
   * When true, always parsing integral number into BigDecial, default is false.
   *
   * @since 4.2.0
   */
  ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL,
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
   * Whether to capture the function arguments(at invocation) into env, the argument list will be
   * stored in __args__ variable in env valid for function body. Default is false(disabled).
   *
   * @since 4.2.0
   */
  CAPTURE_FUNCTION_ARGS,

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
   * Whether to use user passed-in env as top level environment directly.If true, it may make side
   * effects(such as assignment) to user passed-in env., otherwise aviator will wrap the user
   * passed-in env and does not make any side effects into it.
   *
   * Default is false.
   */
  USE_USER_ENV_AS_TOP_ENV_DIRECTLY,

  /**
   * Disable variable assignment when true, default is false. You may want to disable variable
   * assignment for security reason.
   *
   * @since 4.1.2
   */
  DISABLE_ASSIGNMENT;

  private static final boolean TRACE_DEFAULT_VAL =
      Boolean.valueOf(System.getProperty("aviator.asm.trace", "false"));
  private static final boolean TRACE_EVAL_DEFAULT_VAL =
      Boolean.valueOf(System.getProperty("aviator.trace_eval", "false"));

  /**
   * The option's value union
   *
   * @author dennis
   *
   */
  public static class Value {
    public boolean bool;
    public MathContext mathContext;
    public int level;

    public Value(final boolean bool) {
      super();
      this.bool = bool;
    }

    public Value(final MathContext mathContext) {
      super();
      this.mathContext = mathContext;
    }

    public Value(final int level) {
      super();
      this.level = level;
    }

    @Override
    public String toString() {
      return "Value [bool=" + this.bool + ", mathContext=" + this.mathContext + ", level="
          + this.level + "]";
    }


  }

  /**
   * Cast value union into java object.
   *
   * @param val
   * @return
   */
  public Object intoObject(final Value val) {
    switch (this) {
      case ALWAYS_USE_DOUBLE_AS_DECIMAL:
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case TRACE:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
      case DISABLE_ASSIGNMENT:
      case CAPTURE_FUNCTION_ARGS:
        return val.bool;
      case OPTIMIZE_LEVEL: {
        return val.level;
      }
      case MATH_CONTEXT:
        return val.mathContext;
    }
    throw new IllegalArgumentException("Fail to cast value " + val + " for option " + this);
  }

  /**
   * Cast java object into value union.
   *
   * @param val
   * @return
   */
  public Value intoValue(final Object val) {
    switch (this) {
      case ALWAYS_USE_DOUBLE_AS_DECIMAL:
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case TRACE:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
      case CAPTURE_FUNCTION_ARGS:
      case DISABLE_ASSIGNMENT:
        return ((boolean) val) ? TRUE_VALUE : FALSE_VALUE;
      case OPTIMIZE_LEVEL: {
        int level = (int) val;
        if (level == AviatorEvaluator.EVAL) {
          return EVAL_VALUE;
        } else {
          return COMPILE_VALUE;
        }
      }
      case MATH_CONTEXT:
        return new Value((MathContext) val);
    }
    throw new IllegalArgumentException("Fail to cast value " + val + " for option " + this);
  }

  public boolean isValidValue(final Object val) {
    switch (this) {
      case ALWAYS_USE_DOUBLE_AS_DECIMAL:
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case TRACE:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
      case DISABLE_ASSIGNMENT:
      case CAPTURE_FUNCTION_ARGS:
        return val instanceof Boolean;
      case OPTIMIZE_LEVEL:
        return val instanceof Integer && (((Integer) val).intValue() == AviatorEvaluator.EVAL
            || ((Integer) val).intValue() == AviatorEvaluator.COMPILE);
      case MATH_CONTEXT:
        return val instanceof MathContext;
    }
    return false;
  }

  public static final Value FALSE_VALUE = new Value(false);

  public static final Value TRUE_VALUE = new Value(true);

  public static final Value DEFAULT_MATH_CONTEXT = new Value(MathContext.DECIMAL128);

  public static final Value EVAL_VALUE = new Value(AviatorEvaluator.EVAL);

  public static final Value COMPILE_VALUE = new Value(AviatorEvaluator.COMPILE);


  /**
   * Returns the default value of option.
   *
   * @return
   */
  public Object getDefaultValue() {
    return intoObject(getDefaultValueObject());
  }


  /**
   * Returns the default value object of option.
   *
   * @return
   */
  public Value getDefaultValueObject() {
    switch (this) {
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case ALWAYS_USE_DOUBLE_AS_DECIMAL:
        return FALSE_VALUE;
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
        return TRUE_VALUE;
      case NIL_WHEN_PROPERTY_NOT_FOUND:
        return FALSE_VALUE;
      case OPTIMIZE_LEVEL:
        return EVAL_VALUE;
      case MATH_CONTEXT:
        return DEFAULT_MATH_CONTEXT;
      case TRACE_EVAL:
        return TRACE_EVAL_DEFAULT_VAL ? TRUE_VALUE : FALSE_VALUE;
      case TRACE:
        return TRACE_DEFAULT_VAL ? TRUE_VALUE : FALSE_VALUE;
      case PUT_CAPTURING_GROUPS_INTO_ENV:
        return TRUE_VALUE;
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
        return TRUE_VALUE;
      case CAPTURE_FUNCTION_ARGS:
        return FALSE_VALUE;
      case DISABLE_ASSIGNMENT:
        return FALSE_VALUE;
    }
    return null;
  }
}
