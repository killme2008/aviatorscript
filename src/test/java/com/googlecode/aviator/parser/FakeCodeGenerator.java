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

import java.util.List;
import java.util.Stack;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.runtime.FunctionArgument;


/**
 * Fake CodeGenerator,transform infix expression to postfix expression
 *
 * @author dennis
 *
 */
public class FakeCodeGenerator implements CodeGenerator {
  private StringBuffer sb = new StringBuffer();

  private boolean wasFirst = true;

  private final Stack<ScopeInfo> scopes = new Stack<ScopeInfo>();
  private Parser parser;


  @Override
  public void setParser(final Parser parser) {
    this.parser = parser;
  }


  public void reset() {
    this.sb = new StringBuffer();
    this.wasFirst = true;
  }


  @Override
  public Expression getResult() {
    return null;
  }


  public String getPostFixExpression() {
    return this.sb.toString();
  }


  @Override
  public void onAdd(final Token<?> lookhead) {
    appendToken("+");
  }


  @Override
  public void onTernaryEnd(final Token<?> lookhead) {
    appendToken(";");

  }

  private void appendToken(final String s) {
    if (this.wasFirst) {
      this.wasFirst = false;
      this.sb.append(s);
    } else {
      this.sb.append(" ").append(s);
    }
  }


  @Override
  public void onAndLeft(final Token<?> lookhead) {

  }


  @Override
  public void onAndRight(final Token<?> lookhead) {
    appendToken("&&");

  }


  @Override
  public void onJoinRight(final Token<?> lookhead) {
    appendToken("||");
  }


  @Override
  public void onTernaryBoolean(final Token<?> lookhead) {

  }


  @Override
  public void onTernaryLeft(final Token<?> lookhead) {

  }


  @Override
  public void onTernaryRight(final Token<?> lookhead) {
    appendToken("?:");
  }


  @Override
  public void onConstant(final Token<?> lookhead) {
    appendToken(lookhead.getLexeme());
  }


  @Override
  public void onDiv(final Token<?> lookhead) {
    appendToken("/");

  }


  @Override
  public void onEq(final Token<?> lookhead) {
    appendToken("==");

  }


  @Override
  public void onAssignment(final Token<?> lookhead) {
    appendToken("=");
  }


  @Override
  public void onGe(final Token<?> lookhead) {
    appendToken(">=");

  }


  @Override
  public void onGt(final Token<?> lookhead) {
    appendToken(">");

  }


  @Override
  public void onJoinLeft(final Token<?> lookhead) {

  }


  @Override
  public void onLe(final Token<?> lookhead) {
    appendToken("<=");

  }


  @Override
  public void onLt(final Token<?> lookhead) {
    appendToken("<");

  }


  @Override
  public void onMatch(final Token<?> lookhead) {
    appendToken("=~");

  }


  @Override
  public void onMod(final Token<?> lookhead) {
    appendToken("%");

  }


  @Override
  public void onMult(final Token<?> lookhead) {
    appendToken("*");

  }


  @Override
  public void onNeg(final Token<?> lookhead) {
    appendToken("-");

  }


  @Override
  public void onNeq(final Token<?> lookhead) {
    appendToken("!=");

  }


  @Override
  public void onNot(final Token<?> lookhead) {
    appendToken("!");

  }


  @Override
  public void onSub(final Token<?> lookhead) {
    appendToken("-");
  }


  @Override
  public void onMethodInvoke(final Token<?> lookhead, final List<FunctionArgument> params) {
    appendToken("method_invoke<" + (params == null ? "" : params.toString()) + ">");

  }


  @Override
  public void onMethodName(final Token<?> lookhead) {

  }


  @Override
  public void onMethodParameter(final Token<?> lookhead) {

  }


  @Override
  public void onArray(final Token<?> lookhead) {
    appendToken(lookhead.getLexeme());
  }


  @Override
  public void onArrayIndexStart(final Token<?> token) {

  }


  @Override
  public void onArrayIndexEnd(final Token<?> lookhead) {
    appendToken("[]");

  }


  @Override
  public void onBitAnd(final Token<?> lookhead) {
    appendToken("&");

  }


  @Override
  public void onBitNot(final Token<?> lookhead) {
    appendToken("~");

  }


  @Override
  public void onBitOr(final Token<?> lookhead) {
    appendToken("|");

  }


  @Override
  public void onBitXor(final Token<?> lookhead) {
    appendToken("^");

  }


  @Override
  public void onShiftLeft(final Token<?> lookhead) {
    appendToken("<<");

  }

  @Override
  public void onLambdaDefineStart(final Token<?> lookhead) {
    this.scopes.push(this.parser.enterScope());
  }


  @Override
  public void onLambdaBodyStart(final Token<?> lookhead) {

  }


  @Override
  public void onLambdaArgument(final Token<?> lookhead) {}


  @Override
  public void onLambdaBodyEnd(final Token<?> lookhead) {
    appendToken("lambda<defined>");
    this.parser.restoreScope(this.scopes.pop());
  }


  @Override
  public void onShiftRight(final Token<?> lookhead) {
    appendToken(">>");

  }


  @Override
  public void onUnsignedShiftRight(final Token<?> lookhead) {
    appendToken(">>>");

  }

}
