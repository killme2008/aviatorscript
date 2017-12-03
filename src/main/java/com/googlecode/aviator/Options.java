package com.googlecode.aviator;

import java.math.MathContext;


/**
 * Aviator Evaluator Configuration options.
 *
 * @author dennis
 */
public enum Options {
    /**
     * Always use double as BigDecimal, default is false.
     *
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
     * Math context for decimal, default is {@link MathContext#DECIMAL128}
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
     * Whether to enable syntactic sugar for variable name.
     *
     * @since 3.2.0.1
     */
    ENABLE_SYNTACTIC_SUGAR;

    public boolean isValidValue(Object val) {
        switch (this) {
            case ALWAYS_USE_DOUBLE_AS_DECIMAL:
            case TRACE:
                return val instanceof Boolean;
            case OPTIMIZE_LEVEL:
                return val instanceof Integer && (((Integer) val).intValue() == AviatorEvaluator.EVAL
                                                  || ((Integer) val).intValue() == AviatorEvaluator.COMPILE);
            case MATH_CONTEXT:
                return val instanceof MathContext;
            case ENABLE_SYNTACTIC_SUGAR:
                return val instanceof Boolean;
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
            case ALWAYS_USE_DOUBLE_AS_DECIMAL:
                return false;
            case OPTIMIZE_LEVEL:
                return AviatorEvaluator.EVAL;
            case MATH_CONTEXT:
                return MathContext.DECIMAL128;
            case TRACE:
                return Boolean.valueOf(System.getProperty("aviator.asm.trace", "false"));
            case ENABLE_SYNTACTIC_SUGAR:
                return true;
        }
        return null;
    }
}
