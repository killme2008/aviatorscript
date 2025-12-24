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
package com.googlecode.aviator.lexer;

import java.math.BigDecimal;
import java.math.BigInteger;
import java.math.MathContext;
import java.text.CharacterIterator;
import java.text.StringCharacterIterator;
import java.util.LinkedList;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.lexer.token.CharToken;
import com.googlecode.aviator.lexer.token.NumberToken;
import com.googlecode.aviator.lexer.token.StringToken;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.utils.Constants;


/**
 * Expression Lexer,scan tokens from string
 *
 * @author dennis
 *
 */
public class ExpressionLexer {
  private static final long OVERFLOW_FLAG = Long.MAX_VALUE / 10;
  private static final long OVERFLOW_SINGLE = Long.MAX_VALUE % 10;
  // current char
  private char peek;
  // Char iterator for string
  private final CharacterIterator iterator;
  private int lineNo;
  // symbol table
  private final SymbolTable symbolTable;
  // Tokens buffer
  private LinkedList<Token<?>> tokenBuffer;
  private final AviatorEvaluatorInstance instance;
  private final String expression;
  private final MathContext mathContext;
  private final boolean parseFloatIntoDecimal;
  private final boolean parseIntegralNumberIntoDecimal;

  public ExpressionLexer(final AviatorEvaluatorInstance instance, final String expression) {
    this.iterator = new StringCharacterIterator(expression);
    this.expression = expression;
    this.symbolTable = new SymbolTable();
    this.peek = this.iterator.current();
    this.instance = instance;
    this.lineNo = 1;
    this.mathContext = this.instance.getOptionValue(Options.MATH_CONTEXT).mathContext;
    this.parseFloatIntoDecimal =
        this.instance.getOptionValue(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL).bool;
    this.parseIntegralNumberIntoDecimal =
        this.instance.getOptionValue(Options.ALWAYS_PARSE_INTEGRAL_NUMBER_INTO_DECIMAL).bool;

  }



  public SymbolTable getSymbolTable() {
    return this.symbolTable;
  }


  public void setLineNo(final int lineNo) {
    this.lineNo = lineNo;
  }

  public int getLineNo() {
    return this.lineNo;
  }

  /**
   * Push back token
   *
   * @param token
   */
  public void pushback(final Token<?> token) {
    if (this.tokenBuffer == null) {
      this.tokenBuffer = new LinkedList<>();
    }
    this.tokenBuffer.push(token);
  }


  public Token<?> scan() {
    return this.scan(true);
  }


  public void nextChar() {
    this.peek = this.iterator.next();
  }


  public void prevChar() {
    this.peek = this.iterator.previous();
  }

  // Character constants for lexical analysis
  private static final String HEX_CHARS = "0123456789AaBbCcDdEeFf";
  private static final String OPERATOR_CHARS = "=><+-*/%!&|";

  /** Suffix for BigInteger literals, e.g., 100N */
  static final char BIGINT_SUFFIX = 'N';
  /** Suffix for BigDecimal literals, e.g., 3.14M */
  static final char DECIMAL_SUFFIX = 'M';
  /** Hex number prefix after '0', e.g., 0xFF */
  static final char HEX_PREFIX = 'x';


  public boolean isValidHexChar(final char ch) {
    return HEX_CHARS.indexOf(ch) >= 0;
  }


  public int getCurrentIndex() {
    return this.iterator.getIndex();
  }



  public Token<?> scan(final boolean analyse) {
    // If buffer is not empty, return buffered token
    if (this.tokenBuffer != null && !this.tokenBuffer.isEmpty()) {
      return this.tokenBuffer.pop();
    }

    // Skip whitespace or return raw char when not analysing
    for (;; nextChar()) {
      if (this.peek == CharacterIterator.DONE) {
        return null;
      }

      if (analyse) {
        if (this.peek == ' ' || this.peek == '\t' || this.peek == '\r' || this.peek == '\n') {
          if (this.peek == '\n') {
            this.lineNo++;
          }
          continue;
        }
        break;
      } else {
        char ch = this.peek;
        int index = this.iterator.getIndex();
        nextChar();
        return new CharToken(ch, this.lineNo, index);
      }
    }

    // Try each token type in order
    Token<?> token;

    if ((token = scanHexNumber()) != null) {
      return token;
    }
    if ((token = scanNumber()) != null) {
      return token;
    }
    if ((token = scanQuoteVariable(analyse)) != null) {
      return token;
    }
    if ((token = scanVariable()) != null) {
      return token;
    }
    if ((token = scanOperator()) != null) {
      return token;
    }
    if ((token = scanString()) != null) {
      return token;
    }

    // Fallback: return single character token (but not DONE)
    if (this.peek == CharacterIterator.DONE) {
      return null;
    }
    token = new CharToken(this.peek, this.lineNo, this.iterator.getIndex());
    nextChar();
    return token;
  }


  /**
   * Scan hexadecimal number (0x...).
   * 
   * @return NumberToken if hex number found, null otherwise
   */
  private Token<?> scanHexNumber() {
    if (!(Character.isDigit(this.peek) && this.peek == '0')) {
      return null;
    }

    nextChar();
    if (this.peek == HEX_PREFIX || this.peek == 'X') {
      nextChar();
      StringBuilder sb = new StringBuilder();
      int startIndex = this.iterator.getIndex() - 2;
      long value = 0L;
      do {
        sb.append(this.peek);
        value = 16 * value + Character.digit(this.peek, 16);
        nextChar();
      } while (isValidHexChar(this.peek));
      return new NumberToken(value, sb.toString(), this.lineNo, startIndex);
    } else {
      prevChar();
      return null;
    }
  }


  /**
   * Scan decimal number (integer, float, scientific notation, BigInt, BigDecimal).
   * 
   * @return NumberToken or CharToken('.') if found, null otherwise
   */
  private Token<?> scanNumber() {
    if (!(Character.isDigit(this.peek) || this.peek == '.')) {
      return null;
    }

    StringBuilder sb = new StringBuilder();
    int startIndex = this.iterator.getIndex();
    long lval = 0L;
    double dval = 0d;
    double d = 10.0;
    boolean hasDot = false;
    boolean isBigInt = false;
    boolean isBigDecimal = false;
    boolean scientificNotation = false;
    boolean negExp = false;
    boolean isOverflow = false;

    do {
      sb.append(this.peek);

      if (this.peek == '.') {
        if (scientificNotation) {
          throw new CompileExpressionErrorException(
              "Illegal number " + sb + " at " + this.iterator.getIndex());
        }
        if (hasDot) {
          throw new CompileExpressionErrorException(
              "Illegal Number " + sb + " at " + this.iterator.getIndex());
        }
        hasDot = true;
        nextChar();

      } else if (this.peek == BIGINT_SUFFIX) {
        if (hasDot) {
          throw new CompileExpressionErrorException(
              "Illegal number " + sb + " at " + this.iterator.getIndex());
        }
        isBigInt = true;
        nextChar();
        break;

      } else if (this.peek == DECIMAL_SUFFIX) {
        isBigDecimal = true;
        nextChar();
        break;

      } else if (this.peek == 'e' || this.peek == 'E') {
        if (scientificNotation) {
          throw new CompileExpressionErrorException(
              "Illegal number " + sb + " at " + this.iterator.getIndex());
        }
        scientificNotation = true;
        nextChar();
        if (this.peek == '-') {
          negExp = true;
          sb.append(this.peek);
          nextChar();
        }

      } else {
        int digit = Character.digit(this.peek, 10);
        if (scientificNotation) {
          int n = digit;
          nextChar();
          while (Character.isDigit(this.peek)) {
            sb.append(this.peek);
            n = 10 * n + Character.digit(this.peek, 10);
            nextChar();
          }
          while (n-- > 0) {
            if (negExp) {
              dval = dval / 10;
            } else {
              dval = 10 * dval;
            }
          }
          hasDot = true;
        } else if (hasDot) {
          dval = dval + digit / d;
          d = d * 10;
          nextChar();
        } else {
          if (!isOverflow
              && (lval > OVERFLOW_FLAG || (lval == OVERFLOW_FLAG && digit > OVERFLOW_SINGLE))) {
            isOverflow = true;
          }
          lval = 10 * lval + digit;
          dval = 10 * dval + digit;
          nextChar();
        }
      }

    } while (Character.isDigit(this.peek) || this.peek == '.' || this.peek == 'E'
        || this.peek == 'e' || this.peek == DECIMAL_SUFFIX || this.peek == BIGINT_SUFFIX);

    // Build final number value
    Number value;
    if (isBigDecimal) {
      value = new BigDecimal(getBigNumberLexeme(sb), this.mathContext);
    } else if (isBigInt) {
      value = new BigInteger(getBigNumberLexeme(sb));
    } else if (hasDot) {
      if (this.parseFloatIntoDecimal && sb.length() > 1) {
        value = new BigDecimal(sb.toString(), this.mathContext);
      } else if (sb.length() == 1) {
        // Only a dot character
        return new CharToken('.', this.lineNo, startIndex);
      } else {
        value = dval;
      }
    } else {
      if (this.parseIntegralNumberIntoDecimal) {
        value = new BigDecimal(sb.toString(), this.mathContext);
      } else if (isOverflow) {
        value = new BigInteger(sb.toString());
      } else {
        value = lval;
      }
    }

    String lexeme = sb.toString();
    if (isBigDecimal || isBigInt) {
      lexeme = lexeme.substring(0, lexeme.length() - 1);
    }
    return new NumberToken(value, lexeme, this.lineNo, startIndex);
  }


  /**
   * Scan quote variable (#var or #`var`).
   * 
   * @param analyse whether to analyse (for recursive call on comment)
   * @return Variable token if found, null otherwise
   */
  private Token<?> scanQuoteVariable(final boolean analyse) {
    if (this.peek != '#') {
      return null;
    }

    int startIndex = this.iterator.getIndex();
    nextChar(); // skip '#'

    // ## is a comment
    if (this.peek == '#') {
      while (this.peek != CharacterIterator.DONE && this.peek != '\n') {
        nextChar();
      }
      return this.scan(analyse);
    }

    // Check for backquote form #`...`
    boolean hasBackquote = false;
    if (this.peek == '`') {
      hasBackquote = true;
      nextChar();
    }

    StringBuilder sb = new StringBuilder();

    if (hasBackquote) {
      while (this.peek != '`') {
        if (this.peek == CharacterIterator.DONE) {
          throw new CompileExpressionErrorException(
              "EOF while reading string at index: " + this.iterator.getIndex());
        }
        sb.append(this.peek);
        nextChar();
      }
      nextChar(); // skip closing '`'
    } else {
      while (Character.isJavaIdentifierPart(this.peek) || this.peek == '.' || this.peek == '['
          || this.peek == ']') {
        sb.append(this.peek);
        nextChar();
      }
    }

    String lexeme = sb.toString();
    if (lexeme.isEmpty()) {
      throw new ExpressionSyntaxErrorException("Blank variable name after '#'");
    }

    Variable variable = new Variable(lexeme, this.lineNo, startIndex);
    variable.setQuote(true);
    return this.symbolTable.reserve(variable);
  }


  /**
   * Scan normal variable/identifier.
   * 
   * @return Variable token if found, null otherwise
   */
  private Token<?> scanVariable() {
    if (!Character.isJavaIdentifierStart(this.peek)) {
      return null;
    }

    int startIndex = this.iterator.getIndex();
    StringBuilder sb = new StringBuilder();
    boolean hasDot = false;

    do {
      if (this.peek == '.') {
        hasDot = true;
      }
      sb.append(this.peek);
      nextChar();
      // Only allow [] after a dot has been seen (property access syntax)
    } while (Character.isJavaIdentifierPart(this.peek) || this.peek == '.'
        || (hasDot && (this.peek == '[' || this.peek == ']')));

    String lexeme = sb.toString();
    Variable variable = new Variable(lexeme, this.lineNo, startIndex);
    return this.symbolTable.reserve(variable);
  }


  /**
   * Scan operator character.
   * 
   * @return CharToken if operator found, null otherwise
   */
  private Token<?> scanOperator() {
    if (!isBinaryOP(this.peek)) {
      return null;
    }

    CharToken opToken = new CharToken(this.peek, this.lineNo, this.iterator.getIndex());
    nextChar();
    return opToken;
  }


  /**
   * Scan string literal ("..." or '...').
   * 
   * @return StringToken if found, null otherwise
   */
  private Token<?> scanString() {
    if (this.peek != '"' && this.peek != '\'') {
      return null;
    }

    char left = this.peek;
    int startIndex = this.iterator.getIndex();
    StringBuilder sb = new StringBuilder();
    boolean hasInterpolation = false;

    while ((this.peek = this.iterator.next()) != left) {
      // Check for interpolation marker
      if (this.peek == '#' && !hasInterpolation) {
        hasInterpolation = true;
      }

      // Handle escape sequences
      if (this.peek == '\\') {
        nextChar();
        if (this.peek == CharacterIterator.DONE) {
          throw new CompileExpressionErrorException(
              "EOF while reading string at index: " + this.iterator.getIndex());
        }
        if (this.peek == left) {
          sb.append(this.peek);
          continue;
        }
        switch (this.peek) {
          case 't':
            this.peek = '\t';
            break;
          case 'r':
            this.peek = '\r';
            break;
          case 'n':
            this.peek = '\n';
            break;
          case '\\':
            break;
          case 'b':
            this.peek = '\b';
            break;
          case 'f':
            this.peek = '\f';
            break;
          case '#':
            hasInterpolation = true;
            if (this.instance.isFeatureEnabled(Feature.StringInterpolation)) {
              sb.append('\\');
              this.peek = '#';
              break;
            }
          default:
            throw new CompileExpressionErrorException(
                "Unsupported escape character: \\" + this.peek);
        }
      }

      if (this.peek == CharacterIterator.DONE) {
        throw new CompileExpressionErrorException(
            "EOF while reading string at index: " + this.iterator.getIndex());
      }

      sb.append(this.peek);
    }

    nextChar();
    return new StringToken(sb.toString(), this.lineNo, startIndex).withMeta(Constants.INTER_META,
        hasInterpolation);
  }

  public String getScanString() {
    Token<?> firstPushbackToken = this.tokenBuffer != null ? this.tokenBuffer.peekFirst() : null;
    return this.expression.substring(0,
        (firstPushbackToken != null && firstPushbackToken.getStartIndex() > 0)
            ? firstPushbackToken.getEndIndex()
            : this.iterator.getIndex());
  }

  private String getBigNumberLexeme(final StringBuilder sb) {
    String lexeme = sb.toString();
    lexeme = lexeme.substring(0, lexeme.length() - 1);
    return lexeme;
  }

  public static boolean isBinaryOP(final char ch) {
    return OPERATOR_CHARS.indexOf(ch) >= 0;
  }

}
