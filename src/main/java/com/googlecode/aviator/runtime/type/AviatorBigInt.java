package com.googlecode.aviator.runtime.type;

import java.math.BigInteger;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


/**
 * Aviator Big Integer
 * 
 * @since 2.3.0
 * @author dennis<killme2008@gmail.com>
 * 
 */
public class AviatorBigInt extends AviatorLong {

  private static class BigIntCache {
    private BigIntCache() {}

    static final AviatorBigInt cache[] = new AviatorBigInt[256];

    static {
      for (long i = 0; i < cache.length; i++) {
        cache[(int) i] = new AviatorBigInt(BigInteger.valueOf(i - 128));
      }
    }
  }


  public AviatorBigInt(Number number) {
    super(number);
  }


  public static final AviatorBigInt valueOf(BigInteger v) {
    return new AviatorBigInt(v);
  }


  public static final AviatorBigInt valueOf(String v) {
    return new AviatorBigInt(new BigInteger(v));
  }


  public static final AviatorBigInt valueOf(long l) {
    final int offset = 128;
    if (l >= -128 && l <= 127) {
      return BigIntCache.cache[(int) l + offset];
    }
    return valueOf(BigInteger.valueOf(l));
  }


  @Override
  public AviatorObject neg(Map<String, Object> env) {
    return AviatorBigInt.valueOf(this.toBigInt().negate());
  }


  @Override
  public AviatorObject innerSub(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal().subtract(other.toDecimal(), AviatorEvaluator.getMathContext()));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() - other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().subtract(other.toBigInt()));
    }
  }


  @Override
  public AviatorObject innerMult(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal().multiply(other.toDecimal(), AviatorEvaluator.getMathContext()));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() * other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().multiply(other.toBigInt()));
    }
  }


  @Override
  public AviatorObject innerMod(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal().remainder(other.toDecimal(), AviatorEvaluator.getMathContext()));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() % other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().mod(other.toBigInt()));
    }
  }


  @Override
  public AviatorObject innerDiv(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal
            .valueOf(this.toDecimal().divide(other.toDecimal(), AviatorEvaluator.getMathContext()));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() / other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().divide(other.toBigInt()));
    }
  }


  @Override
  public AviatorNumber innerAdd(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal
            .valueOf(this.toDecimal().add(other.toDecimal(), AviatorEvaluator.getMathContext()));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() + other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().add(other.toBigInt()));
    }
  }


  @Override
  public int innerCompare(AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return this.toDecimal().compareTo(other.toDecimal());
      case Double:
        return Double.compare(this.doubleValue(), other.doubleValue());
      default:
        return this.toBigInt().compareTo(other.toBigInt());
    }
  }


  @Override
  protected AviatorObject innerBitAnd(AviatorObject other) {
    return AviatorBigInt.valueOf(this.toBigInt().and(((AviatorNumber) other).toBigInt()));
  }


  @Override
  protected AviatorObject innerBitOr(AviatorObject other) {
    return AviatorBigInt.valueOf(this.toBigInt().or(((AviatorNumber) other).toBigInt()));
  }


  @Override
  protected AviatorObject innerBitXor(AviatorObject other) {
    return AviatorBigInt.valueOf(this.toBigInt().xor(((AviatorNumber) other).toBigInt()));
  }


  @Override
  protected AviatorObject innerShiftLeft(AviatorObject other) {
    this.ensureLong(other);
    return AviatorBigInt
        .valueOf(this.toBigInt().shiftLeft((int) ((AviatorNumber) other).longValue()));
  }


  @Override
  protected AviatorObject innerShiftRight(AviatorObject other) {
    this.ensureLong(other);
    return AviatorBigInt
        .valueOf(this.toBigInt().shiftRight((int) ((AviatorNumber) other).longValue()));
  }


  @Override
  protected AviatorObject innerUnsignedShiftRight(AviatorObject other) {
    return this.innerShiftRight(other);
  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.BigInt;
  }

}
