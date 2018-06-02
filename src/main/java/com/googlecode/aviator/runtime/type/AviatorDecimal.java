package com.googlecode.aviator.runtime.type;

import java.math.BigDecimal;
import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;


/**
 * Aviator Big Decimal
 *
 * @since 2.3.0
 * @author dennis<killme2008@gmail.com>
 *
 */
public class AviatorDecimal extends AviatorNumber {

  public AviatorDecimal(Number number) {
    super(number);
  }


  public static final AviatorDecimal valueOf(BigDecimal d) {
    return new AviatorDecimal(d);
  }


  public static final AviatorDecimal valueOf(Map<String, Object> env, String d) {
    return new AviatorDecimal(new BigDecimal(d, RuntimeUtils.getMathContext(env)));
  }


  @Override
  public AviatorObject innerSub(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() - other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).subtract(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorObject neg(Map<String, Object> env) {
    return AviatorDecimal.valueOf(this.toDecimal(env).negate());
  }


  @Override
  public AviatorObject innerMult(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() * other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).multiply(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorObject innerMod(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() % other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).remainder(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorObject innerDiv(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() / other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).divide(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public AviatorNumber innerAdd(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() + other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).add(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
    }
  }


  @Override
  public int innerCompare(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return Double.compare(this.doubleValue(), other.doubleValue());
      default:
        return this.toDecimal(env).compareTo(other.toDecimal(env));
    }

  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Decimal;
  }

}
