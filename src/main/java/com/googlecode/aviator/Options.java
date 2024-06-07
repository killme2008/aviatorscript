package com.googlecode.aviator;

import java.math.MathContext;
import java.util.Set;
import java.util.concurrent.TimeUnit;
import com.googlecode.aviator.utils.Utils;


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
  ASSIGNABLE_ALLOWED_CLASS_SET,
  /**
   * Script engine evaluate mode, default is ASM mode.
   */
  EVAL_MODE,

  /**
   * Whether the compiled expression is serializable. If true, the compiled expression will
   * implement {@link jva.io.Serializable} and can be encoded/decoded by java serialization.
   */
  SERIALIZABLE,

  /**
   * 
   * The expression execution timeout value in milliseconds. If the execution time exceeds this
   * value, it will throw a {@link com.googlecode.aviator.exception.TimeoutException}. A value of
   * zero or less indicates no timeout limitation, the default value is zero (no limitation). <br/>
   * <br/>
   * Note: this limitation is not strict and may hurt performance, it is only checked before:
   * <ul>
   * <li>Operator evaluating, such as add, sub etc.</li>
   * <li>Jumping in branches, such as loop and conditional clauses etc.</li>
   * <li>Function invocation</li>
   * </ul>
   * 
   * So if the expression doesn't contains these clauses or trapped into a function invocation, the
   * behavior may be not expected. Try its best, but no promises.
   * 
   * @since 5.4.2
   * 
   */
  EVAL_TIMEOUT_MS;


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
    public EvalMode evalMode;
    // Temporal cached number value to avoid expensive calculation.
    public long cachedNumber;

    public Value(final EvalMode evalMode) {
      super();
      this.evalMode = evalMode;
    }

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
      case EVAL_TIMEOUT_MS:
        return val.number;
      case FEATURE_SET:
        return val.featureSet;
      case MATH_CONTEXT:
        return val.mathContext;
      case ALLOWED_CLASS_SET:
      case ASSIGNABLE_ALLOWED_CLASS_SET:
        return val.classes;
      case EVAL_MODE:
        return val.evalMode;
      case SERIALIZABLE:
        return val.bool;
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
      case SERIALIZABLE:
        return ((boolean) val) ? TRUE_VALUE : FALSE_VALUE;
      case OPTIMIZE_LEVEL: {
        int level = (int) val;
        if (level == AviatorEvaluator.EVAL) {
          return EVAL_VALUE;
        } else {
          return COMPILE_VALUE;
        }
      }
      case EVAL_TIMEOUT_MS: {
        Value value = new Value(((Number) val).intValue());
        // Cached the converted result.
        if (value.number > 0) {
          value.cachedNumber = TimeUnit.NANOSECONDS.convert(value.number, TimeUnit.MILLISECONDS);
        }
        return value;
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
      case EVAL_MODE:
        return new Value((EvalMode) val);
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
      case SERIALIZABLE:
        return val instanceof Boolean;
      case FEATURE_SET:
      case ALLOWED_CLASS_SET:
      case ASSIGNABLE_ALLOWED_CLASS_SET:
        return val instanceof Set;
      case OPTIMIZE_LEVEL:
        final int level = ((Integer) val).intValue();
        return val instanceof Integer
            && (level == AviatorEvaluator.EVAL || level == AviatorEvaluator.COMPILE);
      case EVAL_TIMEOUT_MS:
      case MAX_LOOP_COUNT:
        return val instanceof Long || val instanceof Integer;
      case MATH_CONTEXT:
        return val instanceof MathContext;
      case EVAL_MODE:
        return val instanceof EvalMode;
    }
    return false;
  }

  public static final Value FALSE_VALUE = new Value(false);

  public static final Value TRUE_VALUE = new Value(true);

  public static final Value ZERO_VALUE = new Value(0);

  public static final Value DEFAULT_MATH_CONTEXT = new Value(MathContext.DECIMAL128);

  public static final Value EVAL_VALUE = new Value(AviatorEvaluator.EVAL);

  public static final Value COMPILE_VALUE = new Value(AviatorEvaluator.COMPILE);

  private static final Value FULL_FEATURE_SET = new Value(Feature.getFullFeatures());
  private static final boolean TRACE_EVAL_DEFAULT_VAL =
      Boolean.parseBoolean(System.getProperty("aviator.trace_eval", "false"));

  public static final Value ASM_MODE = new Value(EvalMode.ASM);

  public static final Value INTERPRETER_MODE = new Value(EvalMode.INTERPRETER);

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
      case SERIALIZABLE:
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
      case EVAL_MODE:
        return getDefaultEvalMode();
      case EVAL_TIMEOUT_MS:
        return ZERO_VALUE;
    }
    return null;
  }

  public static Value getDefaultEvalMode() {
    if (SYS_EVAL_MODE != null) {
      return SYS_EVAL_MODE;
    }

    return Utils.isAndroid() ? INTERPRETER_MODE : ASM_MODE;
  }


  public static Value SYS_EVAL_MODE = getSystemEvalMode();

  private static Value getSystemEvalMode() {
    String sysProperty = System.getProperty("aviator.eval.mode");
    Value result = null;
    if (sysProperty != null && sysProperty.trim().length() > 0) {
      try {
        EvalMode mode = EvalMode.valueOf(sysProperty.trim().toUpperCase());
        switch (mode) {
          case ASM:
            result = ASM_MODE;
            break;
          case INTERPRETER:
            result = INTERPRETER_MODE;
            break;
          default:
            break;
        }
      } catch (Throwable t) {
        // ignore
      }
    }
    if (result != null) {
      System.out.println("[Aviator INFO] Using " + result.evalMode.name()
          + " eval mode by system property setting aviator.eval.mode");
    }
    return result;
  }
}
