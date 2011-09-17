/**
 *  Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.type;

import java.util.Map;

import com.googlecode.aviator.exception.ExpressionRuntimeException;


/**
 * Aviator long type
 * 
 * @author dennis
 * 
 */
public class AviatorLong extends AviatorNumber {

    private static class LongCache {
        private LongCache() {
        }

        static final AviatorLong cache[] = new AviatorLong[-(-128) + 127 + 1];

        static {
            for (long i = 0; i < cache.length; i++)
                cache[(int)i] = new AviatorLong(i - 128);
        }
    }


    public AviatorLong(Number number) {
        super(number);

    }


    public static AviatorLong valueOf(long l) {
        final int offset = 128;
        if (l >= -128 && l <= 127) { // will cache
            return LongCache.cache[(int) l + offset];
        }
        return new AviatorLong(l);
    }


    public static AviatorLong valueOf(Long l) {
        return valueOf(l.longValue());
    }


    @Override
    public AviatorObject neg(Map<String, Object> env) {
        return AviatorLong.valueOf(-this.number.longValue());
    }


    @Override
    public int innerCompare(AviatorObject other) {
        this.ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        switch (other.getAviatorType()) {
        case Long:
            if (this.number.longValue() > otherNum.longValue()) {
                return 1;
            }
            else if (this.number.longValue() < otherNum.longValue()) {
                return -1;
            }
            else {
                return 0;
            }
        case Double:
            if (this.number.doubleValue() > otherNum.doubleValue()) {
                return 1;
            }
            else if (this.number.doubleValue() < otherNum.doubleValue()) {
                return -1;
            }
            else {
                return 0;
            }
        default:
            throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);
        }

    }


    @Override
    public AviatorObject innerDiv(AviatorObject other) {
        this.ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        switch (other.getAviatorType()) {
        case Long:
            return AviatorLong.valueOf(this.number.longValue() / otherNum.longValue());
        default:
            return new AviatorDouble(this.number.longValue() / otherNum.doubleValue());
        }
    }


    @Override
    public AviatorNumber innerAdd(AviatorNumber other) {
        this.ensureNumber(other);
        AviatorNumber otherNum = other;
        switch (other.getAviatorType()) {
        case Long:
            return AviatorLong.valueOf(this.number.longValue() + otherNum.longValue());
        default:
            return new AviatorDouble(this.number.longValue() + otherNum.doubleValue());
        }
    }


    @Override
    public AviatorObject innerMod(AviatorObject other) {
        this.ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        switch (other.getAviatorType()) {
        case Long:
            return AviatorLong.valueOf(this.number.longValue() % otherNum.longValue());
        default:
            return new AviatorDouble(this.number.longValue() % otherNum.doubleValue());
        }
    }


    @Override
    public AviatorObject innerMult(AviatorObject other) {
        this.ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        switch (other.getAviatorType()) {
        case Long:
            return AviatorLong.valueOf(this.number.longValue() * otherNum.longValue());
        default:
            return new AviatorDouble(this.number.longValue() * otherNum.doubleValue());
        }
    }


    protected void ensureLong(AviatorObject other) {
        if (other.getAviatorType() != AviatorType.Long) {
            throw new ExpressionRuntimeException("Operator only supports Long");
        }
    }


    @Override
    public AviatorObject bitAnd(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Long:
        case Double:
            return this.innerBitAnd(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return this.innerBitAnd(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.bitAnd(other, env);
            }
        default:
            return super.bitAnd(other, env);
        }
    }


    private AviatorObject innerBitAnd(AviatorObject other) {
        this.ensureLong(other);
        AviatorLong otherLong = (AviatorLong) other;
        return AviatorLong.valueOf(this.number.longValue() & otherLong.longValue());
    }


    private AviatorObject innerBitOr(AviatorObject other) {
        this.ensureLong(other);
        AviatorLong otherLong = (AviatorLong) other;
        return AviatorLong.valueOf(this.number.longValue() | otherLong.longValue());
    }


    private AviatorObject innerBitXor(AviatorObject other) {
        this.ensureLong(other);
        AviatorLong otherLong = (AviatorLong) other;
        return AviatorLong.valueOf(this.number.longValue() ^ otherLong.longValue());
    }


    private AviatorObject innerShiftLeft(AviatorObject other) {
        this.ensureLong(other);
        AviatorLong otherLong = (AviatorLong) other;
        return AviatorLong.valueOf(this.number.longValue() << otherLong.longValue());
    }


    private AviatorObject innerShiftRight(AviatorObject other) {
        this.ensureLong(other);
        AviatorLong otherLong = (AviatorLong) other;
        return AviatorLong.valueOf(this.number.longValue() >> otherLong.longValue());
    }


    private AviatorObject innerUnsignedShiftRight(AviatorObject other) {
        this.ensureLong(other);
        AviatorLong otherLong = (AviatorLong) other;
        return AviatorLong.valueOf(this.number.longValue() >>> otherLong.longValue());
    }


    @Override
    public AviatorObject bitNot(Map<String, Object> env) {
        return AviatorLong.valueOf(~this.number.longValue());

    }


    @Override
    public AviatorObject bitOr(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Long:
        case Double:
            return this.innerBitOr(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return this.innerBitOr(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.bitOr(other, env);
            }
        default:
            return super.bitOr(other, env);
        }
    }


    @Override
    public AviatorObject bitXor(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Long:
        case Double:
            return this.innerBitXor(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return this.innerBitXor(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.bitXor(other, env);
            }
        default:
            return super.bitXor(other, env);
        }
    }


    @Override
    public AviatorObject shiftLeft(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Long:
        case Double:
            return this.innerShiftLeft(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return this.innerShiftLeft(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.shiftLeft(other, env);
            }
        default:
            return super.shiftLeft(other, env);
        }
    }


    @Override
    public AviatorObject shiftRight(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Long:
        case Double:
            return this.innerShiftRight(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return this.innerShiftRight(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.shiftRight(other, env);
            }
        default:
            return super.shiftRight(other, env);
        }
    }


    @Override
    public AviatorObject unsignedShiftRight(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Long:
        case Double:
            return this.innerUnsignedShiftRight(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return this.innerUnsignedShiftRight(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.unsignedShiftRight(other, env);
            }
        default:
            return super.unsignedShiftRight(other, env);
        }
    }


    @Override
    public AviatorObject innerSub(AviatorObject other) {
        this.ensureNumber(other);
        AviatorNumber otherNum = (AviatorNumber) other;
        switch (other.getAviatorType()) {
        case Long:
            return AviatorLong.valueOf(this.number.longValue() - otherNum.longValue());
        default:
            return new AviatorDouble(this.number.longValue() - otherNum.doubleValue());
        }
    }


    @Override
    public AviatorType getAviatorType() {
        return AviatorType.Long;
    }

}
