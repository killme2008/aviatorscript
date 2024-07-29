package com.googlecode.aviator.code;

import java.util.Stack;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.parser.ScopeInfo;
import com.googlecode.aviator.runtime.FunctionParam;
import com.googlecode.aviator.utils.Constants;

/**
 * A code generator that generates nothing.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class NoneCodeGenerator implements CodeGenerator {

  private final Stack<ScopeInfo> infos = new Stack<>();
  private Parser parser;

  @Override
  public void onAssignment(final Token<?> lookahead) {


  }

  @Override
  public void setParser(final Parser parser) {
    this.parser = parser;
  }

  @Override
  public void onShiftRight(final Token<?> lookahead) {


  }

  @Override
  public void onShiftLeft(final Token<?> lookahead) {


  }

  @Override
  public void onUnsignedShiftRight(final Token<?> lookahead) {


  }

  @Override
  public void onBitOr(final Token<?> lookahead) {


  }

  @Override
  public void onBitAnd(final Token<?> lookahead) {


  }

  @Override
  public void onBitXor(final Token<?> lookahead) {


  }

  @Override
  public void onBitNot(final Token<?> lookahead) {


  }

  @Override
  public void onAdd(final Token<?> lookahead) {


  }

  @Override
  public void onSub(final Token<?> lookahead) {


  }

  @Override
  public void onMult(final Token<?> lookahead) {


  }



  @Override
  public void onExponent(final Token<?> lookahead) {

  }

  @Override
  public void onDiv(final Token<?> lookahead) {


  }

  @Override
  public void onAndLeft(final Token<?> lookahead) {


  }

  @Override
  public void onAndRight(final Token<?> alookahead) {


  }

  @Override
  public void onTernaryBoolean(final Token<?> lookahead) {


  }

  @Override
  public void onTernaryLeft(final Token<?> lookahead) {


  }

  @Override
  public void onTernaryRight(final Token<?> lookahead) {


  }

  @Override
  public void onTernaryEnd(final Token<?> lookahead) {


  }

  @Override
  public void onJoinLeft(final Token<?> lookahead) {


  }

  @Override
  public void onJoinRight(final Token<?> lookahead) {


  }

  @Override
  public void onEq(final Token<?> lookahead) {


  }

  @Override
  public void onMatch(final Token<?> lookahead) {


  }

  @Override
  public void onNeq(final Token<?> lookahead) {


  }

  @Override
  public void onLt(final Token<?> lookahead) {


  }

  @Override
  public void onLe(final Token<?> lookahead) {


  }

  @Override
  public void onGt(final Token<?> lookahead) {


  }

  @Override
  public void onGe(final Token<?> lookahead) {


  }

  @Override
  public void onMod(final Token<?> lookahead) {


  }

  @Override
  public void onNot(final Token<?> lookahead) {


  }

  @Override
  public void onNeg(final Token<?> lookahead) {


  }

  @Override
  public Expression getResult(final boolean unboxObject) {

    return null;
  }

  @Override
  public void onConstant(final Token<?> lookahead) {


  }

  @Override
  public void onMethodName(final Token<?> lookahead) {


  }

  @Override
  public void onMethodParameter(final Token<?> lookahead) {


  }

  @Override
  public void onMethodInvoke(final Token<?> lookahead) {


  }

  @Override
  public void onLambdaDefineStart(final Token<?> lookahead) {
    Boolean newLexicalScope = lookahead.getMeta(Constants.SCOPE_META, false);
    this.infos.push(this.parser.enterScope(newLexicalScope));
  }

  @Override
  public void onLambdaArgument(final Token<?> lookahead, final FunctionParam param) {


  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookahead) {


  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookahead) {
    this.parser.restoreScope(this.infos.pop());
  }

  @Override
  public void onArray(final Token<?> lookahead) {


  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {


  }

  @Override
  public void onArrayIndexEnd(final Token<?> lookahead) {


  }

}
