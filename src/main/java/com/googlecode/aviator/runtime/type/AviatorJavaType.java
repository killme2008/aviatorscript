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

import java.lang.reflect.Array;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.List;
import java.util.Map;

import org.apache.commons.beanutils.PropertyUtils;

import com.googlecode.aviator.exception.ExpressionRuntimeException;


/**
 * Aviator variable
 * 
 * @author dennis
 * 
 */
public class AviatorJavaType extends AviatorObject {

    final private String name;


    @Override
    public AviatorType getAviatorType() {
        return AviatorType.JavaType;
    }


    public String getName() {
        return name;
    }


    public AviatorJavaType(String name) {
        super();
        this.name = name;
    }


    @Override
    public AviatorObject div(AviatorObject other, Map<String, Object> env) {
        final Object value = getValue(env);
        switch (other.getAviatorType()) {
        case Number:
            if (value instanceof Number) {
                AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
                return aviatorNumber.div(other, env);
            }
            else {
                return super.div(other, env);
            }
        case JavaType:
            if (value instanceof Number) {
                AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
                return thisAviatorNumber.div(other, env);
            }
            else {
                return super.div(other, env);
            }
        default:
            return super.div(other, env);
        }
    }


    @Override
    public Object getValue(Map<String, Object> env) {
        try {
            if (env != null) {
                if (name.contains(".")) {
                    return PropertyUtils.getProperty(env, name);
                }
                else {
                    return env.get(name);
                }
            }
            return null;
        }
        catch (Throwable t) {
            throw new ExpressionRuntimeException("Could not find variable " + name, t);
        }
    }


    @Override
    public AviatorObject mod(AviatorObject other, Map<String, Object> env) {
        final Object value = getValue(env);
        switch (other.getAviatorType()) {
        case Number:
            if (value instanceof Number) {
                AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
                return aviatorNumber.mod(other, env);
            }
            else {
                return super.mod(other, env);
            }
        case JavaType:
            if (value instanceof Number) {
                AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
                return thisAviatorNumber.mod(other, env);
            }
            else {
                return super.mod(other, env);
            }
        default:
            return super.mod(other, env);
        }
    }


    @Override
    public AviatorObject sub(AviatorObject other, Map<String, Object> env) {
        final Object value = getValue(env);
        switch (other.getAviatorType()) {
        case Number:
            if (value instanceof Number) {
                AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
                return aviatorNumber.sub(other, env);
            }
            else {
                return super.sub(other, env);
            }
        case JavaType:
            if (value instanceof Number) {
                AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
                return thisAviatorNumber.sub(other, env);
            }
            else {
                return super.sub(other, env);
            }
        default:
            return super.sub(other, env);
        }
    }


    @Override
    @SuppressWarnings("unchecked")
    public int compare(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case Number:
            AviatorNumber aviatorNumber = (AviatorNumber) other;
            return -aviatorNumber.compare(this, env);
        case String:
            AviatorString aviatorString = (AviatorString) other;
            return -aviatorString.compare(this, env);
        case Boolean:
            AviatorBoolean aviatorBoolean = (AviatorBoolean) other;
            return -aviatorBoolean.compare(this, env);
        case JavaType:

            AviatorJavaType otherJavaType = (AviatorJavaType) other;
            final Object thisValue = getValue(env);
            final Object otherValue = otherJavaType.getValue(env);
            if (thisValue == null) {
                return AviatorNil.NIL.compare(other, env);
            }
            if (thisValue.equals(otherValue)) {
                return 0;
            }
            else {
                if (thisValue instanceof Number) {
                    AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(thisValue);
                    return thisAviatorNumber.compare(other, env);
                }
                else if (thisValue instanceof String) {
                    AviatorString thisAviatorString = new AviatorString((String) thisValue);
                    return thisAviatorString.compare(other, env);
                }
                else if (thisValue instanceof Character) {
                    AviatorString thisAviatorString = new AviatorString(String.valueOf(thisValue));
                    return thisAviatorString.compare(other, env);
                }
                else if (thisValue instanceof Boolean) {
                    AviatorBoolean thisAviatorBoolean = AviatorBoolean.valueOf((Boolean) thisValue);
                    return thisAviatorBoolean.compare(other, env);
                }
                else if (thisValue instanceof Date && otherValue instanceof String) {
                    // This is date,other is string
                    return tryCompareDate(thisValue, otherValue);
                }
                else {
                    try {
                        return ((Comparable) thisValue).compareTo(otherValue);
                    }
                    catch (Throwable t) {
                        throw new ExpressionRuntimeException("Compare " + this + " with " + other + " error", t);
                    }
                }
            }
        case Nil:
            // Any object is greater than nil except nil
            if (this.getValue(env) == null) {
                return 0;
            }
            else {
                return 1;
            }
        default:
            throw new ExpressionRuntimeException("Unknow aviator type");
        }
    }


    private int tryCompareDate(final Object thisValue, final Object otherValue) {
        try {
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS");
            Date otherDate = simpleDateFormat.parse((String) otherValue);
            return ((Date) thisValue).compareTo(otherDate);
        }
        catch (Throwable t) {
            throw new ExpressionRuntimeException("Compare date error", t);
        }
    }


    @Override
    public AviatorObject mult(AviatorObject other, Map<String, Object> env) {
        final Object value = getValue(env);
        switch (other.getAviatorType()) {
        case Number:
            if (value instanceof Number) {
                AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
                return aviatorNumber.mult(other, env);
            }
            else {
                return super.mult(other, env);
            }
        case JavaType:
            if (value instanceof Number) {
                AviatorNumber thisAviatorNumber = AviatorNumber.valueOf(value);
                return thisAviatorNumber.mult(other, env);
            }
            else {
                return super.mult(other, env);
            }
        default:
            return super.mult(other, env);
        }
    }


    @Override
    public AviatorObject neg(Map<String, Object> env) {
        final Object value = getValue(env);
        if (value instanceof Number) {
            return AviatorNumber.valueOf(value).neg(env);
        }
        else {
            return super.neg(env);
        }
    }


    @Override
    public AviatorObject not(Map<String, Object> env) {
        final Object value = getValue(env);
        if (value instanceof Boolean) {
            return AviatorBoolean.valueOf((Boolean) value).not(env);
        }
        else {
            return super.not(env);
        }
    }


    /**
     * Access array or list element
     * 
     * @param env
     * @param indexObject
     * @return
     */
    @SuppressWarnings("unchecked")
    public AviatorObject getElement(Map<String, Object> env, AviatorObject indexObject) {
        Object thisValue = getValue(env);
        if (!thisValue.getClass().isArray() && !(thisValue instanceof List)) {
            throw new ExpressionRuntimeException(this.desc(env)
                    + " is not a array or list,could not use [] to get element");
        }
        Object indexValue = indexObject.getValue(env);
        if (!isInteger(indexValue)) {
            throw new IllegalArgumentException("Illegal index " + indexObject.desc(env));
        }
        int index = ((Number) indexValue).intValue();
        if (thisValue.getClass().isArray()) {
            return new AviatorRuntimeJavaType(Array.get(thisValue, index));
        }
        else {
            return new AviatorRuntimeJavaType(((List) thisValue).get(index));
        }
    }


    private boolean isInteger(Object value) {
        return (value instanceof Long && ((Long) value).longValue() < Integer.MAX_VALUE) || value instanceof Integer
                || value instanceof Short || value instanceof Byte;
    }


    @Override
    public AviatorObject add(AviatorObject other, Map<String, Object> env) {
        final Object value = getValue(env);
        if (value instanceof String) {
            AviatorString aviatorString = new AviatorString((String) value);
            return aviatorString.add(other, env);
        }
        else if (value instanceof Character) {
            AviatorString aviatorString = new AviatorString(String.valueOf(value));
            return aviatorString.add(other, env);
        }
        else if (value instanceof Number) {
            AviatorNumber aviatorNumber = AviatorNumber.valueOf(value);
            return aviatorNumber.add(other, env);
        }
        else if (value instanceof Boolean) {
            return AviatorBoolean.valueOf((Boolean) value).add(other, env);
        }
        else {
            return super.add(other, env);
        }
    }

}
