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

import java.util.Collections;
import java.util.Map;
import java.util.regex.Matcher;
import java.util.regex.Pattern;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * A Aviator regular expression pattern
 *
 * @author dennis
 *
 */
public class AviatorPattern extends AviatorObject {

  final Pattern pattern;


  public AviatorPattern(final String expression) {
    super();
    this.pattern = Pattern.compile(expression);
  }


  public Pattern getPattern() {
    return this.pattern;
  }


  @Override
  public AviatorObject add(final AviatorObject other, final Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case String:
        return new AviatorString(this.pattern.pattern() + ((AviatorString) other).lexeme);
      case JavaType:
        AviatorJavaType javaType = (AviatorJavaType) other;
        final Object otherValue = javaType.getValue(env);
        if (TypeUtils.isString(otherValue)) {
          return new AviatorString(this.pattern.pattern() + otherValue.toString());
        } else {
          return super.add(other, env);
        }
      default:
        return super.add(other, env);

    }
  }


  @Override
  public AviatorObject match(final AviatorObject other, final Map<String, Object> env) {
    switch (other.getAviatorType()) {
      case String:
        AviatorString aviatorString = (AviatorString) other;
        Matcher m = this.pattern.matcher(aviatorString.lexeme);
        if (m.matches()) {
          boolean captureGroups = RuntimeUtils.getInstance(env)
              .getOptionValue(Options.PUT_CAPTURING_GROUPS_INTO_ENV).bool;
          if (captureGroups && env != Collections.EMPTY_MAP) {
            int groupCount = m.groupCount();
            for (int i = 0; i <= groupCount; i++) {
              env.put("$" + i, m.group(i));
            }
          }
          return AviatorBoolean.TRUE;
        } else {
          return AviatorBoolean.FALSE;
        }
      case JavaType:
        AviatorJavaType javaType = (AviatorJavaType) other;
        final Object javaValue = javaType.getValue(env);
        if (TypeUtils.isString(javaValue)) {
          return match(new AviatorString(String.valueOf(javaValue)), env);
        } else {
          throw new ExpressionRuntimeException(desc(env) + " could not match " + other.desc(env));
        }
      default:
        throw new ExpressionRuntimeException(desc(env) + " could not match " + other.desc(env));
    }

  }


  @Override
  public int compare(final AviatorObject other, final Map<String, Object> env) {
    if (this == other) {
      return 0;
    }
    switch (other.getAviatorType()) {
      case Pattern:
        return this.pattern.pattern().compareTo(((AviatorPattern) other).pattern.pattern());
      case JavaType:
        if (other.getValue(env) == null) {
          return 1;
        } else {
          throw new ExpressionRuntimeException("Could not compare Pattern with " + other.desc(env));
        }
      case Nil:
        return 1;
      default:
        throw new ExpressionRuntimeException("Could not compare Pattern with " + other.desc(env));
    }
  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Pattern;
  }


  @Override
  public Object getValue(final Map<String, Object> env) {
    return this.pattern;
  }

}
