package com.googlecode.aviator.runtime.type;

import java.math.BigDecimal;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.runtime.RuntimeUtils;


/**
 * Aviator Big Decimal
 *
 * @since 2.3.0
 * @author dennis<killme2008@gmail.com>
 *
 */
public class AviatorDecimal extends AviatorNumber {

  public AviatorDecimal(final Number number) {
    super(number);
  }


  public static final AviatorDecimal valueOf(final BigDecimal d) {
    return new AviatorDecimal(d);
  }


  public static final AviatorDecimal valueOf(final Map<String, Object> env, final String d) {
    return new AviatorDecimal(new BigDecimal(d, RuntimeUtils.getMathContext(env)));
  }


  public static final AviatorDecimal valueOf(final AviatorEvaluatorInstance instance,
      final String d) {
    return new AviatorDecimal(
        new BigDecimal(d, instance.getOptionValue(Options.MATH_CONTEXT).mathContext));
  }

  @Override
  public AviatorObject innerSub(final Map<String, Object> env, final AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(doubleValue() - other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            toDecimal(env).subtract(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorObject neg(final Map<String, Object> env) {
    return AviatorDecimal.valueOf(toDecimal(env).negate());
  }


  @Override
  public AviatorObject innerMult(final Map<String, Object> env, final AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(doubleValue() * other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            toDecimal(env).multiply(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorObject innerMod(final Map<String, Object> env, final AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(doubleValue() % other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            toDecimal(env).remainder(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorObject innerDiv(final Map<String, Object> env, final AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(doubleValue() / other.doubleValue());
      default:
        return AviatorDecimal
            .valueOf(toDecimal(env).divide(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorNumber innerAdd(final Map<String, Object> env, final AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(doubleValue() + other.doubleValue());
      default:
        return AviatorDecimal
            .valueOf(toDecimal(env).add(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public int innerCompare(final Map<String, Object> env, final AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return Double.compare(doubleValue(), other.doubleValue());
      default:
        return toDecimal(env).compareTo(other.toDecimal(env));
    }

  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Decimal;
  }

}
