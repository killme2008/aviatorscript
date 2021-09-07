package com.googlecode.aviator;

import java.math.MathContext;
import java.util.Set;


/**
 * Aviator Evaluator Configuration options.
 *
 * @author dennis
 *
 */
public enum Options {

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
   *
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
   * Default is true.
   */
  USE_USER_ENV_AS_TOP_ENV_DIRECTLY,


  /**
   * Max loop count to prevent too much CPU consumption. If it's value is zero or negative, it means
   * no limitation on loop count.Default is zero.
   */
  MAX_LOOP_COUNT,
  /**
   * AviatorScript engine feature set, see {@link Feature}
   *
   * @since 5.
   */
  FEATURE_SET,

  /**
   * Allowed java class set in new statement and class's static method(fields) etc. It's null by
   * default. Null ALLOWED_CLASS_SET and ASSIGNABLE_ALLOWED_CLASS_SET means all classes are allowed
   * (default); Empty ALLOWED_CLASS_SET or ASSIGNABLE_ALLOWED_CLASS_SET means forbidding all
   * classes.
   *
   * @since 5.2.2
   */
  ALLOWED_CLASS_SET,

  /**
   * Allowed assignable java class set in new statement and class's static method(fields) etc. It's
   * null by default. Null ALLOWED_CLASS_SET and ASSIGNABLE_ALLOWED_CLASS_SET means all classes are
   * allowed (default); Empty ALLOWED_CLASS_SET or ASSIGNABLE_ALLOWED_CLASS_SET means forbidding all
   * classes.
   */
  ASSIGNABLE_ALLOWED_CLASS_SET;


  /**
   * The option's value union
   *
   * @author dennis
   *
   */
  public static class Value {
    public boolean bool;
    public MathContext mathContext;
    public int number;
    public Set<Feature> featureSet;
    public Set<Class<?>> classes;

    public Value() {
      super();
    }

    static Value fromClasses(final Set<Class<?>> classes) {
      Value v = new Value();
      v.classes = classes;
      return v;
    }

    public Value(final Set<Feature> featureSet) {
      super();
      this.featureSet = featureSet;
    }

    public Value(final boolean bool) {
      super();
      this.bool = bool;
    }

    public Value(final MathContext mathContext) {
      super();
      this.mathContext = mathContext;
    }

    public Value(final int n) {
      super();
      this.number = n;
    }

    @Override
    public String toString() {
      return "Value [bool=" + this.bool + ", mathContext=" + this.mathContext + ", number="
          + this.number + ", featureSet=" + this.featureSet + ", classes=" + this.classes + "]";
    }
  }

  /**
   * Cast value union into java object.
   *
   * @param val
   * @return
   */
  public Object intoObject(final Value val) {
    if (val == null) {
      return null;
    }
    switch (this) {
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
      case CAPTURE_FUNCTION_ARGS:
        return val.bool;
      case MAX_LOOP_COUNT:
      case OPTIMIZE_LEVEL:
        return val.number;
      case FEATURE_SET:
        return val.featureSet;
      case MATH_CONTEXT:
        return val.mathContext;
      case ALLOWED_CLASS_SET:
      case ASSIGNABLE_ALLOWED_CLASS_SET:
        return val.classes;
    }
    throw new IllegalArgumentException("Fail to cast value " + val + " for option " + this);
  }

  /**
   * Cast java object into value union.
   *
   * @param val
   * @return
   */
  @SuppressWarnings("unchecked")
  public Value intoValue(final Object val) {
    switch (this) {
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
      case CAPTURE_FUNCTION_ARGS:
        return ((boolean) val) ? TRUE_VALUE : FALSE_VALUE;
      case OPTIMIZE_LEVEL: {
        int level = (int) val;
        if (level == AviatorEvaluator.EVAL) {
          return EVAL_VALUE;
        } else if (level == AviatorEvaluator.INTERPRET) {
          return INTERPRET_VALUE;
        } else {
          return COMPILE_VALUE;
        }
      }
      case MAX_LOOP_COUNT:
        return new Value(((Number) val).intValue());
      case ALLOWED_CLASS_SET:
      case ASSIGNABLE_ALLOWED_CLASS_SET:
        return Value.fromClasses((Set<Class<?>>) val);
      case FEATURE_SET:
        return new Value((Set<Feature>) val);
      case MATH_CONTEXT:
        return new Value((MathContext) val);
    }
    throw new IllegalArgumentException("Fail to cast value " + val + " for option " + this);
  }

  public boolean isValidValue(final Object val) {
    switch (this) {
      case ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL:
      case ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL:
      case TRACE_EVAL:
      case PUT_CAPTURING_GROUPS_INTO_ENV:
      case ENABLE_PROPERTY_SYNTAX_SUGAR:
      case NIL_WHEN_PROPERTY_NOT_FOUND:
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
      case CAPTURE_FUNCTION_ARGS:
        return val instanceof Boolean;
      case FEATURE_SET:
      case ALLOWED_CLASS_SET:
      case ASSIGNABLE_ALLOWED_CLASS_SET:
        return val instanceof Set;
      case OPTIMIZE_LEVEL:
        final int level = ((Integer) val).intValue();
        return val instanceof Integer && (level == AviatorEvaluator.EVAL
            || level == AviatorEvaluator.COMPILE || level == AviatorEvaluator.INTERPRET);
      case MAX_LOOP_COUNT:
        return val instanceof Long || val instanceof Integer;
      case MATH_CONTEXT:
        return val instanceof MathContext;
    }
    return false;
  }

  public static final Value FALSE_VALUE = new Value(false);

  public static final Value TRUE_VALUE = new Value(true);

  public static final Value ZERO_VALUE = new Value(0);

  public static final Value DEFAULT_MATH_CONTEXT = new Value(MathContext.DECIMAL128);

  public static final Value EVAL_VALUE = new Value(AviatorEvaluator.EVAL);

  public static final Value COMPILE_VALUE = new Value(AviatorEvaluator.COMPILE);

  public static final Value INTERPRET_VALUE = new Value(AviatorEvaluator.INTERPRET);

  private static final Value FULL_FEATURE_SET = new Value(Feature.getFullFeatures());
  private static final boolean TRACE_EVAL_DEFAULT_VAL =
      Boolean.parseBoolean(System.getProperty("aviator.trace_eval", "false"));

  public static final Value NULL_CLASS_SET = Value.fromClasses(null);


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
      case PUT_CAPTURING_GROUPS_INTO_ENV:
        return TRUE_VALUE;
      case USE_USER_ENV_AS_TOP_ENV_DIRECTLY:
        return TRUE_VALUE;
      case CAPTURE_FUNCTION_ARGS:
        return FALSE_VALUE;
      case MAX_LOOP_COUNT:
        return ZERO_VALUE;
      case FEATURE_SET:
        return FULL_FEATURE_SET;
      case ALLOWED_CLASS_SET:
      case ASSIGNABLE_ALLOWED_CLASS_SET:
        return NULL_CLASS_SET;
    }
    return null;
  }
}
