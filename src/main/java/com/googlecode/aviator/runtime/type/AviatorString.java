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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;

import com.googlecode.aviator.exception.ExpressionRuntimeException;


/**
 * A aviator string
 * 
 * @author dennis
 * 
 */
public class AviatorString extends AviatorObject {
    final String lexeme;


    @Override
    public AviatorType getAviatorType() {
        return AviatorType.String;
    }


    @Override
    public Object getValue(Map<String, Object> env) {
        return this.lexeme;
    }


    public AviatorString(String lexeme) {
        super();
        this.lexeme = lexeme;
    }


    @Override
    public AviatorObject add(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case String:
            AviatorString otherString = (AviatorString) other;
            return new AviatorString(this.lexeme + otherString.lexeme);
        case Boolean:
            AviatorBoolean otherBoolean = (AviatorBoolean) other;
            return new AviatorString(this.lexeme + otherBoolean.value);
        case Number:
            AviatorNumber otherNumber = (AviatorNumber) other;
            return new AviatorString(this.lexeme + otherNumber.number);
        case Nil:
        case JavaType:
            return new AviatorString(this.lexeme + other.getValue(env));
        case Pattern:
            AviatorPattern otherPatterh = (AviatorPattern) other;
            return new AviatorString(this.lexeme + otherPatterh.pattern.pattern());

        default:
            return super.add(other, env);
        }
    }


    private int tryCompareDate(final Date otherDate) {
        try {
            SimpleDateFormat simpleDateFormat = new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS");
            Date thisDate = simpleDateFormat.parse(this.lexeme);
            return thisDate.compareTo(otherDate);
        }
        catch (Throwable t) {
            throw new ExpressionRuntimeException("Compare date error", t);
        }
    }


    @Override
    public int compare(AviatorObject other, Map<String, Object> env) {
        switch (other.getAviatorType()) {
        case String:
            AviatorString otherString = (AviatorString) other;
            return this.lexeme.compareTo(otherString.lexeme);
        case JavaType:
            AviatorJavaType javaType = (AviatorJavaType) other;
            final Object otherJavaValue = javaType.getValue(env);
            if (otherJavaValue == null) {
                return 1;
            }
            if (otherJavaValue instanceof String) {
                return this.lexeme.compareTo((String) otherJavaValue);
            }
            else if (otherJavaValue instanceof Character) {
                return this.lexeme.compareTo(String.valueOf(otherJavaValue));
            }
            else if (otherJavaValue instanceof Date) {
                return tryCompareDate((Date) otherJavaValue);
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


    public String getLexeme() {
        return lexeme;
    }

}
