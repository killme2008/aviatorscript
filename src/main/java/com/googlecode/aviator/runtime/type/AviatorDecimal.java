package com.googlecode.aviator.runtime.type;

import java.math.BigDecimal;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


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


  public static final AviatorDecimal valueOf(String d) {
    return new AviatorDecimal(new BigDecimal(d, AviatorEvaluator.getMathContext()));
  }


  @Override
  public AviatorObject innerSub(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() - other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal().subtract(other.toDecimal(), AviatorEvaluator.getMathContext()));
    }
  }


  @Override
  public AviatorObject neg(Map<String, Object> env) {
    return AviatorDecimal.valueOf(this.toDecimal().negate());
  }


  @Override
  public AviatorObject innerMult(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() * other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal().multiply(other.toDecimal(), AviatorEvaluator.getMathContext()));
    }
  }


  @Override
  public AviatorObject innerMod(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() % other.doubleValue());
      default:
        return AviatorDecimal.valueOf(
            this.toDecimal().remainder(other.toDecimal(), AviatorEvaluator.getMathContext()));
    }
  }


  @Override
  public AviatorObject innerDiv(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() / other.doubleValue());
      default:
        return AviatorDecimal
            .valueOf(this.toDecimal().divide(other.toDecimal(), AviatorEvaluator.getMathContext()));
    }
  }


  @Override
  public AviatorNumber innerAdd(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() + other.doubleValue());
      default:
        return AviatorDecimal
            .valueOf(this.toDecimal().add(other.toDecimal(), AviatorEvaluator.getMathContext()));
    }
  }


  @Override
  public int innerCompare(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Double:
        return Double.compare(this.doubleValue(), other.doubleValue());
      default:
        return this.toDecimal().compareTo(other.toDecimal());
    }

  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Decimal;
  }

}
