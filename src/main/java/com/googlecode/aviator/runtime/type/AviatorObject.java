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
 * Aviator root object
 * 
 * @author dennis
 * 
 */
public abstract class AviatorObject {
    public abstract int compare(AviatorObject other, Map<String, Object> env);


    public abstract AviatorType getAviatorType();


    public AviatorObject match(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException(desc(env) + " doesn't support match operation '=~'");
    }


    public AviatorObject neg(Map<String, Object> env) {
        throw new ExpressionRuntimeException(desc(env) + " doesn't support negative operation '-'");
    }


    public AviatorObject not(Map<String, Object> env) {
        throw new ExpressionRuntimeException(desc(env) + " doesn't support not operation '!'");
    }


    public String desc(Map<String, Object> env) {
        return this.getAviatorType() + "(" + this.getValue(env) + ")";
    }


    public abstract Object getValue(Map<String, Object> env);


    public AviatorObject add(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException("Could not add " + this.desc(env) + " with " + other.desc(env));
    }


    public AviatorObject sub(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException("Could not sub " + this.desc(env) + " with " + other.desc(env));
    }


    public AviatorObject mod(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException("Could not mod " + this.desc(env) + " with " + other.desc(env));
    }


    public AviatorObject div(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException("Could not div " + this.desc(env) + " with " + other.desc(env));
    }


    public AviatorObject mult(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException("Could not mult " + this.desc(env) + " with " + other.desc(env));
    }


    public Number numberValue(Map<String, Object> env) {
        if (!(this.getValue(env) instanceof Number)) {
            throw new ExpressionRuntimeException(this.desc(env) + " is not a number value");
        }
        return (Number) this.getValue(env);
    }


    public String stringValue(Map<String, Object> env) {
        if (!(this.getValue(env) instanceof String) && !(this.getValue(env) instanceof Character)) {
            throw new ExpressionRuntimeException(this.desc(env) + " is not a string value");
        }
        return String.valueOf(this.getValue(env));
    }


    public boolean booleanValue(Map<String, Object> env) {
        if (!(this.getValue(env) instanceof Boolean)) {
            throw new ExpressionRuntimeException(this.desc(env) + " is not a boolean value");
        }
        return (Boolean) this.getValue(env);
    }
}
