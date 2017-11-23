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
 * Operator token
 * 
 * @author dennis
 * 
 */
public class OperatorToken extends AbstractToken<OperatorType> {

  private final OperatorType operatorType;


  public OperatorType getOperatorType() {
    return operatorType;
  }


  public OperatorToken(int startIndex, OperatorType operatorType) {
    super(startIndex, operatorType.getToken());
    this.operatorType = operatorType;
  }


  public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
    return TokenType.Operator;
  }


  public OperatorType getValue(Map<String, Object> env) {
    return operatorType;
  }

}
