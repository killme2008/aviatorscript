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
 * Charactor token
 * 
 * @author dennis
 * 
 */
public class CharToken extends AbstractToken<Character> {
  private final char ch;
  private int startIndex;


  public char getCh() {
    return ch;
  }


  public CharToken(char peek, int startIndex) {
    super(startIndex, String.valueOf(peek));
    this.ch = peek;
  }


  @Override
  public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
    return TokenType.Char;
  }


  @Override
  public Character getValue(Map<String, Object> env) {
    return ch;
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = super.hashCode();
    result = prime * result + ch;
    result = prime * result + startIndex;
    return result;
  }


  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (!super.equals(obj)) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    CharToken other = (CharToken) obj;
    if (ch != other.ch) {
      return false;
    }
    if (startIndex != other.startIndex) {
      return false;
    }
    return true;
  }

}
