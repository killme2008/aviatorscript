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
  private final String lexeme;

  @Override
  public AviatorType getAviatorType() {
    return AviatorType.String;
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    return getLexeme();
  }

  public AviatorString(final String lexeme) {
    super();
    this.lexeme = lexeme;
  }

  @Override
  public AviatorObject add(final AviatorObject other, final Map<String, Object> env) {
    final StringBuilder sb = new StringBuilder(this.lexeme);

    if (other.getAviatorType() == AviatorType.Pattern) {
      final AviatorPattern otherPatterh = (AviatorPattern) other;
      sb.append(otherPatterh.pattern.pattern());
    } else {
      sb.append(other.getValue(env));
    }
    return new AviatorStringBuilder(sb);
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
      final Date thisDate = simpleDateFormat.parse(getLexeme());
      return thisDate.compareTo(otherDate);
    } catch (final Throwable t) {
      throw new ExpressionRuntimeException("Compare date error", t);
    }
  }

  @Override
  public int compare(final AviatorObject other, final Map<String, Object> env) {
    if (this == other) {
      return 0;
    }
    switch (other.getAviatorType()) {
      case String:
        final AviatorString otherString = (AviatorString) other;
        if (getLexeme() == null && otherString.getLexeme() != null) {
          return -1;
        } else if (getLexeme() != null && otherString.getLexeme() == null) {
          return 1;
        } else if (getLexeme() == null && otherString.getLexeme() == null) {
          return 0;
        } else {
          return getLexeme().compareTo(otherString.getLexeme());
        }
      case JavaType:
        final AviatorJavaType javaType = (AviatorJavaType) other;
        final Object otherJavaValue = javaType.getValue(env);
        if (getLexeme() == null && otherJavaValue == null) {
          return 0;
        } else if (getLexeme() != null && otherJavaValue == null) {
          return 1;
        }
        if (TypeUtils.isString(otherJavaValue)) {
          if (getLexeme() == null) {
            return -1;
          } else {
            return getLexeme().compareTo(String.valueOf(otherJavaValue));
          }
        } else if (otherJavaValue instanceof Date) {
          return tryCompareDate((Date) otherJavaValue);
        } else {
          throw new ExpressionRuntimeException(
              "Could not compare " + desc(env) + " with " + other.desc(env));
        }
      case Nil:
        if (getLexeme() == null) {
          return 0;
        } else {
          return 1;
        }
      default:
        throw new ExpressionRuntimeException(
            "Could not compare " + desc(env) + " with " + other.desc(env));
    }
  }

  public String getLexeme() {
    return this.lexeme;
  }


}
