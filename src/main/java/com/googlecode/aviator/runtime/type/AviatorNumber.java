/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.type;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * Aviator number type
 *
 * @author dennis
 *
 */
public abstract class AviatorNumber extends AviatorObject {
  protected Number number;


  public AviatorNumber(Number number) {
    super();
    this.number = number;
  }


  @Override
  public Object getValue(Map<String, Object> env) {
    return this.number;
  }


  public static AviatorNumber valueOf(Object value) {
    if (TypeUtils.isLong(value)) {
      return AviatorLong.valueOf(((Number) value).longValue());
    } else if (TypeUtils.isDouble(value)) {
      return new AviatorDouble(((Number) value).doubleValue());
    } else if (TypeUtils.isBigInt(value)) {
      return AviatorBigInt.valueOf((BigInteger) value);
    } else if (TypeUtils.isDecimal(value)) {
      return AviatorDecimal.valueOf((BigDecimal) value);
    } else {
      throw new ClassCastException("Could not cast " + value.getClass().getName() + " to Number");
    }

  }


  public double doubleValue() {
    return this.number.doubleValue();
  }


  @Override
  public AviatorObject add(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case String:
        return new AviatorString(this.number.toString() + ((AviatorString) other).getLexeme());
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return this.innerAdd(env, (AviatorNumber) other);
      case JavaType:
        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object otherValue = otherJavaType.getValue(env);
        if (otherValue instanceof Number) {
          return this.innerAdd(env, AviatorNumber.valueOf(otherValue));
        } else if (otherValue instanceof String) {
          return new AviatorString(this.number.toString() + otherValue);
        } else {
          return super.add(other, env);
        }
      default:
        return super.add(other, env);
    }

  }


  @Override
  public AviatorObject sub(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return this.innerSub(env, (AviatorNumber) other);
      case JavaType:
        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object otherValue = otherJavaType.getValue(env);
        if (otherValue instanceof Number) {
          return this.innerSub(env, AviatorNumber.valueOf(otherValue));
        } else {
          return super.sub(other, env);
        }
      default:
        return super.sub(other, env);
    }

  }


  @Override
  public AviatorObject mod(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return this.innerMod(env, (AviatorNumber) other);
      case JavaType:
        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object otherValue = otherJavaType.getValue(env);
        if (otherValue instanceof Number) {
          return this.innerMod(env, AviatorNumber.valueOf(otherValue));
        } else {
          return super.mod(other, env);
        }
      default:
        return super.mod(other, env);
    }
  }


  @Override
  public AviatorObject div(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return this.innerDiv(env, (AviatorNumber) other);
      case JavaType:
        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object otherValue = otherJavaType.getValue(env);
        if (otherValue instanceof Number) {
          return this.innerDiv(env, AviatorNumber.valueOf(otherValue));
        } else {
          return super.div(other, env);
        }
      default:
        return super.div(other, env);
    }

  }


  @Override
  public AviatorObject mult(AviatorObject other, Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return this.innerMult(env, (AviatorNumber) other);
      case JavaType:
        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object otherValue = otherJavaType.getValue(env);
        if (otherValue instanceof Number) {
          return this.innerMult(env, AviatorNumber.valueOf(otherValue));
        } else {
          return super.mult(other, env);
        }
      default:
        return super.mult(other, env);
    }

  }


  @Override
  public int compare(AviatorObject other, Map<String, Object> env) {
    if (this == other) {
      return 0;
    }
    switch (other.getAviatorType()) {
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        return this.innerCompare(env, (AviatorNumber) other);
      case JavaType:
        AviatorJavaType otherJavaType = (AviatorJavaType) other;
        final Object otherValue = otherJavaType.getValue(env);
        if (otherValue == null) {
          return 1;
        }
        if (otherValue instanceof Number) {
          return this.innerCompare(env, AviatorNumber.valueOf(otherValue));
        } else {
          throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);
        }
      case Nil:
        return 1;
      default:
        throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);

    }
  }


  public abstract AviatorObject innerSub(Map<String, Object> env, AviatorNumber other);


  public abstract AviatorObject innerMult(Map<String, Object> env, AviatorNumber other);


  public abstract AviatorObject innerMod(Map<String, Object> env, AviatorNumber other);


  public abstract AviatorObject innerDiv(Map<String, Object> env, AviatorNumber other);


  public abstract AviatorObject innerAdd(Map<String, Object> env, AviatorNumber other);


  public abstract int innerCompare(Map<String, Object> env, AviatorNumber other);


  public long longValue() {
    return this.number.longValue();
  }


  public final BigInteger toBigInt() {
    if (TypeUtils.isBigInt(this.number)) {
      return (BigInteger) this.number;
    } else {
      return new BigInteger(String.valueOf(this.number.longValue()));
    }
  }


  public final BigDecimal toDecimal(Map<String, Object> env) {
    if (TypeUtils.isDecimal(this.number)) {
      return (BigDecimal) this.number;
    } else if (TypeUtils.isBigInt(this.number)) {
      return new BigDecimal(this.toBigInt());
    } else {
      return new BigDecimal(this.number.doubleValue(), RuntimeUtils.getMathContext(env));
    }
  }
}
