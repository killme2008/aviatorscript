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

import java.util.HashMap;
import java.util.Map;

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
  private Map<String, Object> metaMap;


  @Override
  public Map<String, Object> getMetaMap() {
    return this.metaMap;
  }


  public void setMetaMap(final Map<String, Object> metaMap) {
    this.metaMap = metaMap;
  }


  @Override
  public Token<T> withMeta(final String name, final Object v) {
    if (this.metaMap == null) {
      this.metaMap = new HashMap<String, Object>();
    }
    this.metaMap.put(name, v);
    return this;
  }


  @Override
  public <V> V getMeta(final String name, final V defaultVal) {
    if (this.metaMap == null) {
      return defaultVal;
    }
    V val = (V) this.metaMap.get(name);
    if (val == null) {
      return defaultVal;
    }
    return val;
  }


  public AbstractToken(final int startIndex, final String lexeme) {
    super();
    this.startIndex = startIndex;
    this.lexeme = lexeme;
  }


  @Override
  public String getLexeme() {
    return this.lexeme;
  }


  @Override
  public int getStartIndex() {
    return this.startIndex;
  }


  @Override
  public String toString() {
    return "[type='" + getType().name() + "',lexeme='" + getLexeme() + "',index=" + this.startIndex
        + "]";
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.lexeme == null) ? 0 : this.lexeme.hashCode());
    result = prime * result + this.startIndex;
    return result;
  }


  @Override
  public boolean equals(final Object obj) {
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
    if (this.lexeme == null) {
      if (other.lexeme != null) {
        return false;
      }
    } else if (!this.lexeme.equals(other.lexeme)) {
      return false;
    }
    if (this.startIndex != other.startIndex) {
      return false;
    }
    return true;
  }

}
