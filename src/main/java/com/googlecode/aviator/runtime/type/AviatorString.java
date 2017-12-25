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

import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.utils.TypeUtils;

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
        final AviatorString otherString = (AviatorString) other;
        return new AviatorString(this.lexeme + otherString.lexeme);
      case Boolean:
        final AviatorBoolean otherBoolean = (AviatorBoolean) other;
        return new AviatorString(this.lexeme + otherBoolean.value);
      case BigInt:
      case Decimal:
      case Long:
      case Double:
        final AviatorNumber otherNumber = (AviatorNumber) other;
        return new AviatorString(this.lexeme + otherNumber.number);
      case Nil:
      case JavaType:
        return new AviatorString(this.lexeme + other.getValue(env));
      case Pattern:
        final AviatorPattern otherPatterh = (AviatorPattern) other;
        return new AviatorString(this.lexeme + otherPatterh.pattern.pattern());

      default:
        return super.add(other, env);
    }
  }

  private static final ThreadLocal<SimpleDateFormat> DATE_FORMATTER =
      new ThreadLocal<SimpleDateFormat>() {

        @Override
        protected SimpleDateFormat initialValue() {
          return new SimpleDateFormat("yyyy-MM-dd HH:mm:ss:SS");
        }

      };

  private int tryCompareDate(final Date otherDate) {
    try {
      final SimpleDateFormat simpleDateFormat = DATE_FORMATTER.get();
      final Date thisDate = simpleDateFormat.parse(this.lexeme);
      return thisDate.compareTo(otherDate);
    } catch (final Throwable t) {
      throw new ExpressionRuntimeException("Compare date error", t);
    }
  }

  @Override
  public int compare(AviatorObject other, Map<String, Object> env) {
    if (this == other) {
      return 0;
    }
    switch (other.getAviatorType()) {
      case String:
        final AviatorString otherString = (AviatorString) other;
        if (this.lexeme == null && otherString.lexeme != null) {
          return -1;
        } else if (this.lexeme != null && otherString.lexeme == null) {
          return 1;
        } else if (this.lexeme == null && otherString.lexeme == null) {
          return 0;
        } else {
          return this.lexeme.compareTo(otherString.lexeme);
        }
      case JavaType:
        final AviatorJavaType javaType = (AviatorJavaType) other;
        final Object otherJavaValue = javaType.getValue(env);
        if (this.lexeme == null && otherJavaValue == null) {
          return 0;
        } else if (this.lexeme != null && otherJavaValue == null) {
          return 1;
        }
        if (TypeUtils.isString(otherJavaValue)) {
          if (this.lexeme == null) {
            return -1;
          } else {
            return this.lexeme.compareTo(String.valueOf(otherJavaValue));
          }
        } else if (otherJavaValue instanceof Date) {
          return this.tryCompareDate((Date) otherJavaValue);
        } else {
          throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);
        }
      case Nil:
        if (this.lexeme == null) {
          return 0;
        } else {
          return 1;
        }
      default:
        throw new ExpressionRuntimeException("Could not compare " + this + " with " + other);
    }
  }

  public String getLexeme() {
    return this.lexeme;
  }

}
