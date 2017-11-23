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

/**
 * Base token class
 * 
 * @author dennis
 * 
 * @param <T>
 */
public abstract class AbstractToken<T> implements Token<T> {

  private final int startIndex;

  protected final String lexeme;


  public AbstractToken(int startIndex, String lexeme) {
    super();
    this.startIndex = startIndex;
    this.lexeme = lexeme;
  }


  public String getLexeme() {
    return this.lexeme;
  }


  public int getStartIndex() {
    return this.startIndex;
  }


  @Override
  public String toString() {
    return "[type='" + getType().name() + "',lexeme='" + getLexeme() + "',index=" + startIndex
        + "]";
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((lexeme == null) ? 0 : lexeme.hashCode());
    result = prime * result + startIndex;
    return result;
  }


  @Override
  public boolean equals(Object obj) {
    if (this == obj) {
      return true;
    }
    if (obj == null) {
      return false;
    }
    if (getClass() != obj.getClass()) {
      return false;
    }
    AbstractToken<?> other = (AbstractToken<?>) obj;
    if (lexeme == null) {
      if (other.lexeme != null) {
        return false;
      }
    } else if (!lexeme.equals(other.lexeme)) {
      return false;
    }
    if (startIndex != other.startIndex) {
      return false;
    }
    return true;
  }

}
