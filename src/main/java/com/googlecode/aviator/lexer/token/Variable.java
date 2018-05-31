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
package com.googlecode.aviator.lexer.token;

import java.util.Map;


/**
 * Variable token
 *
 * @author dennis
 *
 */
public class Variable extends AbstractToken<Object> {

  public static final String INSTANCE_VAR = "__instance__";
  public static final String ENV_VAR = "__env__";

  public boolean isQuote() {
    return quote;
  }


  public void setQuote(boolean special) {
    this.quote = special;
  }

  private boolean quote = false;

  /**
   * Boolean value true
   */
  public static final Variable TRUE = new Variable("true", -1) {

    @Override
    public Object getValue(Map<String, Object> env) {
      return true;
    }

  };

  /**
   * Boolean value false
   */
  public static final Variable FALSE = new Variable("false", -1) {

    @Override
    public Object getValue(Map<String, Object> env) {
      return false;
    }

  };

  /**
   * Boolean value false
   */
  public static final Variable NIL = new Variable("nil", -1) {

    @Override
    public Object getValue(Map<String, Object> env) {
      return null;
    }

  };


  /**
   * Lambda keyword
   */
  public static final Variable LAMBDA = new Variable("lambda", -1) {

    @Override
    public Object getValue(Map<String, Object> env) {
      return false;
    }

  };


  @Override
  public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
    return TokenType.Variable;
  }


  @Override
  public Object getValue(Map<String, Object> env) {
    if (env != null) {
      return env.get(this.lexeme);
    } else {
      return this.lexeme;
    }
  }


  public Variable(String name, int startIndex) {
    super(startIndex, name);
  }


  @Override
  public String toString() {
    String index = ",index=" + getStartIndex();
    if (getStartIndex() == -1) {
      index = "";
    }
    return "[type='variable',lexeme='" + getLexeme() + "'" + index + "]";
  }

}
