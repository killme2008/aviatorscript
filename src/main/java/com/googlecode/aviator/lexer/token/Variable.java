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


  private static final long serialVersionUID = -2444861882394614240L;

  public boolean isQuote() {
    return this.quote;
  }


  public void setQuote(final boolean special) {
    this.quote = special;
  }

  private boolean quote = false;

  /**
   * Boolean value true
   */
  public static final Variable TRUE = new Variable("true", 0, -1) {


    private static final long serialVersionUID = 4935383811479451467L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return true;
    }

  };

  /**
   * Boolean value false
   */
  public static final Variable FALSE = new Variable("false", 0, -1) {


    private static final long serialVersionUID = -5580561244268123057L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return false;
    }

  };

  /**
   * Boolean value false
   */
  public static final Variable NIL = new Variable("nil", 0, -1) {


    private static final long serialVersionUID = 5405079118962650113L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return null;
    }

  };


  /**
   * Lambda keyword
   */
  public static final Variable LAMBDA = new Variable("lambda", 0, -1) {


    private static final long serialVersionUID = 5239852591646934183L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * end keyword
   */
  public static final Variable END = new Variable("end", 0, -1) {


    private static final long serialVersionUID = 1381194991295087667L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * if keyword
   */
  public static final Variable IF = new Variable("if", 0, -1) {


    private static final long serialVersionUID = 371294588236388238L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * else keyword
   */
  public static final Variable ELSE = new Variable("else", 0, -1) {


    private static final long serialVersionUID = -8680881360673170940L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };


  /**
   * for keyword
   */
  public static final Variable FOR = new Variable("for", 0, -1) {


    private static final long serialVersionUID = -8264017729568522971L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * in keyword
   */
  public static final Variable IN = new Variable("in", 0, -1) {


    private static final long serialVersionUID = -4622016350260111762L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * return keyword
   */
  public static final Variable RETURN = new Variable("return", 0, -1) {


    private static final long serialVersionUID = 720528094830222144L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * return keyword
   */
  public static final Variable BREAK = new Variable("break", 0, -1) {


    private static final long serialVersionUID = -3811349935845126666L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * return keyword
   */
  public static final Variable CONTINUE = new Variable("continue", 0, -1) {


    private static final long serialVersionUID = -97078099593768562L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * let keyword
   */
  public static final Variable LET = new Variable("let", 0, -1) {


    private static final long serialVersionUID = 8947676572495899744L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * while keyword
   */
  public static final Variable WHILE = new Variable("while", 0, -1) {


    private static final long serialVersionUID = -7452208104495691948L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * fn keyword
   */
  public static final Variable FN = new Variable("fn", 0, -1) {


    private static final long serialVersionUID = 1310362304034338211L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * elsif keyword
   */
  public static final Variable ELSIF = new Variable("elsif", 0, -1) {


    private static final long serialVersionUID = 9179033352817183568L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  /**
   * elsif keyword
   */
  public static final Variable TRY = new Variable("try", 0, -1) {

    private static final long serialVersionUID = -5941442994484723465L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  public static final Variable CATCH = new Variable("catch", 0, -1) {

    private static final long serialVersionUID = 5648899950149965053L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  public static final Variable FINALLY = new Variable("finally", 0, -1) {

    private static final long serialVersionUID = 4620910964773812463L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  public static final Variable THROW = new Variable("throw", 0, -1) {

    private static final long serialVersionUID = 113057346952612067L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };


  public static final Variable NEW = new Variable("new", 0, -1) {

    private static final long serialVersionUID = 113057346952612067L;

    @Override
    public Object getValue(final Map<String, Object> env) {
      return this;
    }

  };

  @Override
  public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
    return TokenType.Variable;
  }


  @Override
  public Object getValue(final Map<String, Object> env) {
    if (env != null) {
      return env.get(this.lexeme);
    } else {
      return this.lexeme;
    }
  }


  public Variable(final String name, final int lineNo, final int startIndex) {
    super(name, lineNo, startIndex);
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
