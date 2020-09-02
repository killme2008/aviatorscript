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
 * A Number token
 *
 * @author dennis
 *
 */
public class NumberToken extends AbstractToken<Number> {


  private static final long serialVersionUID = 3787410200228564680L;
  private Number value;


  public NumberToken(final Number value, final String lexeme) {
    super(lexeme, 0, -1);
    this.value = value;
  }


  public NumberToken(final Number value, final String lexeme, final int lineNo,
      final int startIndex) {
    super(lexeme, lineNo, startIndex);
    this.value = value;
  }


  public void setNumber(final Number number) {
    this.value = number;
  }


  public Number getNumber() {
    return this.value;
  }


  @Override
  public Number getValue(final Map<String, Object> env) {
    return this.value;
  }


  @Override
  public TokenType getType() {
    return TokenType.Number;
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ((this.value == null) ? 0 : this.value.hashCode());
    return result;
  }


  @Override
  public boolean equals(final Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    NumberToken other = (NumberToken) obj;
    if (this.value == null) {
      if (other.value != null) {
        return false;
      }
    } else if (!this.value.equals(other.value)) {
      return false;
    }
    return true;
  }

}
