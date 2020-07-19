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
import java.text.StringCharacterIterator;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.ExpressionLexer;
import com.googlecode.aviator.lexer.token.CharToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Token.TokenType;
import com.googlecode.aviator.parser.ExpressionParser;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.utils.TypeUtils;

/**
 * A aviator string
 *
 * @author dennis
 *
 */
public class AviatorString extends AviatorObject {

  private static final long serialVersionUID = -7430694306919959899L;
  private final String lexeme;

  private static interface StringSegment {
    StringBuilder appendTo(StringBuilder sb, Map<String, Object> env);
  }

  private static class LiteralSegment implements StringSegment {
    String literal;


    public LiteralSegment(final String literal) {
      super();
      this.literal = literal;
    }


    @Override
    public String toString() {
      return "LiteralSegment [literal=" + this.literal + "]";
    }


    @Override
    public StringBuilder appendTo(final StringBuilder sb, final Map<String, Object> env) {
      return sb.append(this.literal);
    }
  }

  private static class ExpressionSegment implements StringSegment {
    Expression exp;

    public ExpressionSegment(final Expression exp) {
      super();
      this.exp = exp;
    }

    @Override
    public StringBuilder appendTo(final StringBuilder sb, final Map<String, Object> env) {
      return sb.append(this.exp.execute(env));
    }

    @Override
    public String toString() {
      return "ExpressionSegment [exp=" + this.exp + "]";
    }

  }

  private List<StringSegment> segments;



  @Override
  public AviatorType getAviatorType() {
    return AviatorType.String;
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    return getLexeme(env);
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

  private int tryCompareDate(final Map<String, Object> env, final Date otherDate) {
    try {
      final SimpleDateFormat simpleDateFormat = DATE_FORMATTER.get();
      final Date thisDate = simpleDateFormat.parse(getLexeme(env));
      return thisDate.compareTo(otherDate);
    } catch (final Throwable t) {
      throw new ExpressionRuntimeException("Compare date error", t);
    }
  }

  @Override
  public int innerCompare(final AviatorObject other, final Map<String, Object> env) {
    if (this == other) {
      return 0;
    }
    switch (other.getAviatorType()) {
      case String:
        final AviatorString otherString = (AviatorString) other;
        if (getLexeme(env) == null && otherString.getLexeme(env) != null) {
          return -1;
        } else if (getLexeme(env) != null && otherString.getLexeme(env) == null) {
          return 1;
        } else if (getLexeme(env) == null && otherString.getLexeme(env) == null) {
          return 0;
        } else {
          return getLexeme(env).compareTo(otherString.getLexeme(env));
        }
      case JavaType:
        final AviatorJavaType javaType = (AviatorJavaType) other;
        final Object otherJavaValue = javaType.getValue(env);
        if (getLexeme(env) == null && otherJavaValue == null) {
          return 0;
        } else if (getLexeme(env) != null && otherJavaValue == null) {
          return 1;
        }
        if (TypeUtils.isString(otherJavaValue)) {
          if (getLexeme(env) == null) {
            return -1;
          } else {
            return getLexeme(env).compareTo(String.valueOf(otherJavaValue));
          }
        } else if (otherJavaValue instanceof Date) {
          return tryCompareDate(env, (Date) otherJavaValue);
        } else {
          throw new ExpressionRuntimeException(
              "Could not compare " + desc(env) + " with " + other.desc(env));
        }
      case Nil:
        if (getLexeme(env) == null) {
          return 0;
        } else {
          return 1;
        }
      default:
        throw new ExpressionRuntimeException(
            "Could not compare " + desc(env) + " with " + other.desc(env));
    }
  }

  public String getLexeme(final Map<String, Object> env) {
    AviatorEvaluatorInstance engine = RuntimeUtils.getInstance(env);
    if (!engine.isFeatureEnabled(Feature.StringInterpolation) || this.lexeme == null
        || this.lexeme.length() < 3) {
      return this.lexeme;
    }

    if (this.segments == null) {
      List<StringSegment> segs = new ArrayList<AviatorString.StringSegment>();

      StringBuilder sb = new StringBuilder(this.lexeme.length());
      StringCharacterIterator it = new StringCharacterIterator(this.lexeme);
      char ch = it.current(), prev = StringCharacterIterator.DONE;
      int lastInterPos = 0;
      int i = 1;
      for (;;) {
        if (ch == '#' && prev != '\\') {
          prev = ch;
          ch = it.next();
          i++;
          if (ch == '{') {
            // interpolation position.
            if (i - 2 > lastInterPos) {
              segs.add(new LiteralSegment(this.lexeme.substring(lastInterPos, i - 2)));
            }
            ExpressionLexer lexer = new ExpressionLexer(engine, this.lexeme.substring(i));
            ExpressionParser parser =
                new ExpressionParser(engine, lexer, engine.newCodeGenerator(false));

            Expression exp = parser.parse(false);
            final Token<?> lookhead = parser.getLookhead();
            if (lookhead.getType() != TokenType.Char || ((CharToken) lookhead).getCh() != '}') {
              parser.reportSyntaxError("expect '}' to complete string interpolation");
            }
            int expStrLen = lookhead.getStartIndex() + 1;
            while (expStrLen-- > 0) {
              prev = ch;
              i++;
              ch = it.next();
            }
            sb.append(exp.execute(env));
            segs.add(new ExpressionSegment(exp));
            lastInterPos = i;
          } else {
            sb.append(prev);
            sb.append(ch);
          }
        } else {
          sb.append(ch);
        }

        prev = ch;

        ch = it.next();
        i++;
        if (ch == StringCharacterIterator.DONE) {
          if (i - 1 > lastInterPos) {
            segs.add(new LiteralSegment(this.lexeme.substring(lastInterPos, i - 1)));
          }
          break;
        }
      }
      // cache string segments to speedup.
      this.segments = segs;
      return sb.toString();
    } else {
      StringBuilder sb = new StringBuilder(this.lexeme.length());
      for (StringSegment ss : this.segments) {
        ss.appendTo(sb, env);
      }
      return sb.toString();
    }
  }


}
