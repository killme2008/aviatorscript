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
        if (value instanceof Long || value instanceof Byte || value instanceof Short || value instanceof Integer) {
            return new AviatorLong(((Number) value).longValue());

        }
        else if (value instanceof Double || value instanceof Float) {
            return new AviatorDouble(((Number) value).doubleValue());
        }
        else {
            throw new ClassCastException("Could not cast " + value.getClass().getName() + " to Number");
        }

    }


    public double doubleValue() {
        return number.doubleValue();
    }


    @Override
    public AviatorObject add(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case String:
            return new AviatorString(this.number.toString() + ((AviatorString) other).getLexeme());
        case Number:
            return innerAdd((AviatorNumber) other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return innerAdd(AviatorNumber.valueOf(otherValue));
            }
            else if (otherValue instanceof String) {
                return new AviatorString(this.number.toString() + otherValue);
            }
            else {
                return super.add(other, env);
            }
        default:
            return super.add(other, env);
        }

    }


    @Override
    public AviatorObject sub(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Number:
            return innerSub(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return innerSub(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.sub(other, env);
            }
        default:
            return super.sub(other, env);
        }

    }


    @Override
    public AviatorObject mod(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Number:
            return innerMod(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return innerMod(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.mod(other, env);
            }
        default:
            return super.mod(other, env);
        }
    }


    @Override
    public AviatorObject div(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Number:
            return innerDiv(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return innerDiv(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.div(other, env);
            }
        default:
            return super.div(other, env);
        }

    }


    @Override
    public AviatorObject mult(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Number:
            return innerMult(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue instanceof Number) {
                return innerMult(AviatorNumber.valueOf(otherValue));
            }
            else {
                return super.mult(other, env);
            }
        default:
            return super.mult(other, env);
        }

    }


    @Override
    public int compare(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Number:
            return innerCompare(other);
        case JavaType:
            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object otherValue = otherJavaType.getValue(env);
            if (otherValue == null) {
                return 1;
            }
            if (otherValue instanceof Number) {
                return innerCompare(AviatorNumber.valueOf(otherValue));
            }
            else {
                throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);
            }
        case Nil:
            return 1;
        default:
            throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);

        }
    }


    public abstract AviatorObject innerSub(AviatorObject other);


    public abstract AviatorObject innerMult(AviatorObject other);


    public abstract AviatorObject innerMod(AviatorObject other);


    public abstract AviatorObject innerDiv(AviatorObject other);


    public abstract AviatorNumber innerAdd(AviatorNumber other);


    public abstract int innerCompare(AviatorObject other);


    @Override
    public AviatorType getAviatorType() {
        return AviatorType.Number;
    }


    public long longValue() {
        return number.longValue();
    }


    protected void ensureNumber(AviatorObject other) {
        if (other.getAviatorType() != AviatorType.Number) {
            throw new ExpressionRuntimeException("Operator only supports Number");
        }
    }
}
