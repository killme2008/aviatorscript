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
package com.googlecode.aviator.code;

import com.googlecode.aviator.Expression;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.runtime.FunctionParam;


/**
 * Code generator interface
 *
 * @author dennis
 *
 */
public interface CodeGenerator {

  public void onAssignment(Token<?> lookahead);

  public void setParser(Parser parser);

  public void onShiftRight(Token<?> lookahead);


  public void onShiftLeft(Token<?> lookahead);


  public void onUnsignedShiftRight(Token<?> lookahead);


  public void onBitOr(Token<?> lookahead);


  public void onBitAnd(Token<?> lookahead);


  public void onBitXor(Token<?> lookahead);


  public void onBitNot(Token<?> lookahead);


  public void onAdd(Token<?> lookahead);


  public void onSub(Token<?> lookahead);


  public void onMult(Token<?> lookahead);

  public void onExponent(Token<?> loohead);


  public void onDiv(Token<?> lookahead);


  public void onAndLeft(Token<?> lookahead);


  public void onAndRight(Token<?> alookahead);


  public void onTernaryBoolean(Token<?> lookahead);


  public void onTernaryLeft(Token<?> lookahead);


  public void onTernaryRight(Token<?> lookahead);

  public void onTernaryEnd(Token<?> lookahead);


  public void onJoinLeft(Token<?> lookahead);


  public void onJoinRight(Token<?> lookahead);


  public void onEq(Token<?> lookahead);


  public void onMatch(Token<?> lookahead);


  public void onNeq(Token<?> lookahead);


  public void onLt(Token<?> lookahead);


  public void onLe(Token<?> lookahead);


  public void onGt(Token<?> lookahead);


  public void onGe(Token<?> lookahead);


  public void onMod(Token<?> lookahead);


  public void onNot(Token<?> lookahead);


  public void onNeg(Token<?> lookahead);

  public Expression getResult(boolean unboxObject);

  public void onConstant(Token<?> lookahead);

  public void onMethodName(Token<?> lookahead);

  public void onMethodParameter(Token<?> lookahead);

  public void onMethodInvoke(Token<?> lookahead);

  public void onLambdaDefineStart(Token<?> lookahead);

  public void onLambdaArgument(Token<?> lookahead, FunctionParam param);

  public void onLambdaBodyStart(Token<?> lookahead);

  public void onLambdaBodyEnd(Token<?> lookahead);

  public void onArray(Token<?> lookahead);

  public void onArrayIndexStart(Token<?> token);

  public void onArrayIndexEnd(Token<?> lookahead);
}
