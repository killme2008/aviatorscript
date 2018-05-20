package com.googlecode.aviator.runtime.type;

import java.math.BigInteger;
import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;


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
  public AviatorObject innerSub(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).subtract(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() - other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().subtract(other.toBigInt()));
    }
  }


  @Override
  public AviatorObject innerMult(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).multiply(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() * other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().multiply(other.toBigInt()));
    }
  }


  @Override
  public AviatorObject innerMod(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).remainder(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() % other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().mod(other.toBigInt()));
    }
  }


  @Override
  public AviatorObject innerDiv(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).divide(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() / other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().divide(other.toBigInt()));
    }
  }


  @Override
  public AviatorNumber innerAdd(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return AviatorDecimal.valueOf(
            this.toDecimal(env).add(other.toDecimal(env), RuntimeUtils.getMathContext(env)));
      case Double:
        return AviatorDouble.valueOf(this.doubleValue() + other.doubleValue());
      default:
        return AviatorBigInt.valueOf(this.toBigInt().add(other.toBigInt()));
    }
  }


  @Override
  public int innerCompare(Map<String, Object> env, AviatorNumber other) {
    switch (other.getAviatorType()) {
      case Decimal:
        return this.toDecimal(env).compareTo(other.toDecimal(env));
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
