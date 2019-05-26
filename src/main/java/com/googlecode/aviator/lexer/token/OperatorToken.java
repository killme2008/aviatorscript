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

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.FunctionArgument;


/**
 * Operator token
 *
 * @author dennis
 *
 */
public class OperatorToken extends AbstractToken<OperatorType> {

  private final OperatorType operatorType;

  private List<FunctionArgument> params;



  public List<FunctionArgument> getParams() {
    return this.params;
  }


  public void setParams(final List<FunctionArgument> params) {
    this.params = params;
  }


  public OperatorType getOperatorType() {
    return this.operatorType;
  }


  public OperatorToken(final int startIndex, final OperatorType operatorType) {
    super(startIndex, operatorType.getToken());
    this.operatorType = operatorType;
  }


  @Override
  public com.googlecode.aviator.lexer.token.Token.TokenType getType() {
    return TokenType.Operator;
  }


  @Override
  public OperatorType getValue(final Map<String, Object> env) {
    return this.operatorType;
  }

}
