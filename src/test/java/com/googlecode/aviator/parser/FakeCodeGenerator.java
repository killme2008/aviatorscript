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
package com.googlecode.aviator.parser;

import com.googlecode.aviator.Expression;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.lexer.token.Token;


/**
 * Fake CodeGenerator,transform infix expression to postfix expression
 * 
 * @author dennis
 * 
 */
public class FakeCodeGenerator implements CodeGenerator {
  private StringBuffer sb = new StringBuffer();

  private boolean wasFirst = true;


  public void reset() {
    this.sb = new StringBuffer();
    this.wasFirst = true;
  }


  public Expression getResult() {
    return null;
  }


  public String getPostFixExpression() {
    return this.sb.toString();
  }


  public void onAdd(Token<?> lookhead) {
    this.appendToken("+");
  }


  private void appendToken(String s) {
    if (this.wasFirst) {
      this.wasFirst = false;
      this.sb.append(s);
    } else {
      this.sb.append(" ").append(s);
    }
  }


  public void onAndLeft(Token<?> lookhead) {

  }


  public void onAndRight(Token<?> lookhead) {
    this.appendToken("&&");

  }


  public void onJoinRight(Token<?> lookhead) {
    this.appendToken("||");
  }


  public void onTernaryBoolean(Token<?> lookhead) {

  }


  public void onTernaryLeft(Token<?> lookhead) {

  }


  public void onTernaryRight(Token<?> lookhead) {
    this.appendToken("?:");
  }


  public void onConstant(Token<?> lookhead) {
    this.appendToken(lookhead.getLexeme());
  }


  public void onDiv(Token<?> lookhead) {
    this.appendToken("/");

  }


  public void onEq(Token<?> lookhead) {
    this.appendToken("==");

  }


  public void onGe(Token<?> lookhead) {
    this.appendToken(">=");

  }


  public void onGt(Token<?> lookhead) {
    this.appendToken(">");

  }


  public void onJoinLeft(Token<?> lookhead) {

  }


  public void onLe(Token<?> lookhead) {
    this.appendToken("<=");

  }


  public void onLt(Token<?> lookhead) {
    this.appendToken("<");

  }


  public void onMatch(Token<?> lookhead) {
    this.appendToken("=~");

  }


  public void onMod(Token<?> lookhead) {
    this.appendToken("%");

  }


  public void onMult(Token<?> lookhead) {
    this.appendToken("*");

  }


  public void onNeg(Token<?> lookhead) {
    this.appendToken("-");

  }


  public void onNeq(Token<?> lookhead) {
    this.appendToken("!=");

  }


  public void onNot(Token<?> lookhead) {
    this.appendToken("!");

  }


  public void onSub(Token<?> lookhead) {
    this.appendToken("-");
  }


  public void onMethodInvoke(Token<?> lookhead) {
    this.appendToken("method<invoked>");

  }


  public void onMethodName(Token<?> lookhead) {

  }


  public void onMethodParameter(Token<?> lookhead) {

  }


  public void onArray(Token<?> lookhead) {
    this.appendToken(lookhead.getLexeme());
  }


  public void onArrayIndexStart(Token<?> token) {

  }


  public void onArrayIndexEnd(Token<?> lookhead) {
    this.appendToken("[]");

  }


  public void onBitAnd(Token<?> lookhead) {
    this.appendToken("&");

  }


  public void onBitNot(Token<?> lookhead) {
    this.appendToken("~");

  }


  public void onBitOr(Token<?> lookhead) {
    this.appendToken("|");

  }


  public void onBitXor(Token<?> lookhead) {
    this.appendToken("^");

  }


  public void onShiftLeft(Token<?> lookhead) {
    this.appendToken("<<");

  }


  public void onShiftRight(Token<?> lookhead) {
    this.appendToken(">>");

  }


  public void onUnsignedShiftRight(Token<?> lookhead) {
    this.appendToken(">>>");

  }

}
