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

import java.io.Serializable;
import java.util.IdentityHashMap;
import java.util.Map;

/**
 * Base token class
 *
 * @author dennis
 *
 * @param <T>
 */
public abstract class AbstractToken<T> implements Token<T> {

  private static final long serialVersionUID = 4498841242745542399L;

  private final int lineIndex;

  private final int lineNo;


  @Override
  public int getLineNo() {
    return this.lineNo;
  }


  protected String lexeme;
  private Map<String, Object> metaMap;


  @Override
  public Map<String, Object> getMetaMap() {
    return this.metaMap;
  }


  public void setMetaMap(final Map<String, Object> metaMap) {
    assert (metaMap == null || metaMap instanceof IdentityHashMap);
    this.metaMap = metaMap;
  }


  @Override
  public Token<T> withMeta(final String name, final Object v) {
    if (this.metaMap == null) {
      this.metaMap = new IdentityHashMap<>();
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

  @Override
  public <V> V getMeta(final String name) {
    if (this.metaMap == null) {
      return null;
    }
    return (V) this.metaMap.get(name);
  }


  public AbstractToken(final String lexeme, final int lineNo, final int lineIdex) {
    super();
    this.lineNo = lineNo;
    this.lineIndex = lineIdex;
    this.lexeme = lexeme;
  }


  @Override
  public String getLexeme() {
    return this.lexeme;
  }


  @Override
  public int getStartIndex() {
    return this.lineIndex;
  }

  @Override
  public int getEndIndex() {
    return this.lineIndex + (this.lexeme != null ? this.lexeme.length() : 0);
  }


  @Override
  public String toString() {
    return "[type='" + getType().name() + "',lexeme='" + getLexeme() + "',index=" + this.lineIndex
        + "]";
  }


  @Override
  public int hashCode() {
    final int prime = 31;
    int result = 1;
    result = prime * result + ((this.lexeme == null) ? 0 : this.lexeme.hashCode());
    result = prime * result + this.lineIndex;
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
    if (this.lineIndex != other.lineIndex) {
      return false;
    }
    return true;
  }

}
