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
import java.util.Map;


/**
 * Lex token interface
 *
 * @author dennis
 *
 * @param <T>
 */
public interface Token<T> extends Serializable {
  enum TokenType {
    String, Variable, Number, Char, Operator, Pattern, Delegate
  }

  Token<T> withMeta(String name, Object v);

  Map<String, Object> getMetaMap();

  <V> V getMeta(final String name, final V defaultVal);

  <V> V getMeta(final String name);

  T getValue(Map<String, Object> env);

  TokenType getType();


  String getLexeme();


  int getStartIndex();

  int getEndIndex();

  int getLineNo();
}
