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


/**
 * Code generator interface
 * 
 * @author dennis
 * 
 */
public interface CodeGenerator {

  public void onShiftRight(Token<?> lookhead);


  public void onShiftLeft(Token<?> lookhead);


  public void onUnsignedShiftRight(Token<?> lookhead);


  public void onBitOr(Token<?> lookhead);


  public void onBitAnd(Token<?> lookhead);


  public void onBitXor(Token<?> lookhead);


  public void onBitNot(Token<?> lookhead);


  public void onAdd(Token<?> lookhead);


  public void onSub(Token<?> lookhead);


  public void onMult(Token<?> lookhead);


  public void onDiv(Token<?> lookhead);


  public void onAndLeft(Token<?> lookhead);


  public void onAndRight(Token<?> lookhead);


  public void onTernaryBoolean(Token<?> lookhead);


  public void onTernaryLeft(Token<?> lookhead);


  public void onTernaryRight(Token<?> lookhead);


  public void onJoinLeft(Token<?> lookhead);


  public void onJoinRight(Token<?> lookhead);


  public void onEq(Token<?> lookhead);


  public void onMatch(Token<?> lookhead);


  public void onNeq(Token<?> lookhead);


  public void onLt(Token<?> lookhead);


  public void onLe(Token<?> lookhead);


  public void onGt(Token<?> lookhead);


  public void onGe(Token<?> lookhead);


  public void onMod(Token<?> lookhead);


  public void onNot(Token<?> lookhead);


  public void onNeg(Token<?> lookhead);


  public Expression getResult();


  public void onConstant(Token<?> lookhead);


  public void onMethodName(Token<?> lookhead);


  public void onMethodParameter(Token<?> lookhead);


  public void onMethodInvoke(Token<?> lookhead);


  public void onArray(Token<?> lookhead);


  public void onArrayIndexStart(Token<?> token);


  public void onArrayIndexEnd(Token<?> lookhead);

}
