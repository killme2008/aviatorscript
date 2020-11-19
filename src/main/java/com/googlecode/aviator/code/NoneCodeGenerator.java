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
  public void onAssignment(final Token<?> lookhead) {


  }

  @Override
  public void setParser(final Parser parser) {
    this.parser = parser;
  }

  @Override
  public void onShiftRight(final Token<?> lookhead) {


  }

  @Override
  public void onShiftLeft(final Token<?> lookhead) {


  }

  @Override
  public void onUnsignedShiftRight(final Token<?> lookhead) {


  }

  @Override
  public void onBitOr(final Token<?> lookhead) {


  }

  @Override
  public void onBitAnd(final Token<?> lookhead) {


  }

  @Override
  public void onBitXor(final Token<?> lookhead) {


  }

  @Override
  public void onBitNot(final Token<?> lookhead) {


  }

  @Override
  public void onAdd(final Token<?> lookhead) {


  }

  @Override
  public void onSub(final Token<?> lookhead) {


  }

  @Override
  public void onMult(final Token<?> lookhead) {


  }



  @Override
  public void onExponent(final Token<?> lookhead) {

  }

  @Override
  public void onDiv(final Token<?> lookhead) {


  }

  @Override
  public void onAndLeft(final Token<?> lookhead) {


  }

  @Override
  public void onAndRight(final Token<?> lookhead) {


  }

  @Override
  public void onTernaryBoolean(final Token<?> lookhead) {


  }

  @Override
  public void onTernaryLeft(final Token<?> lookhead) {


  }

  @Override
  public void onTernaryRight(final Token<?> lookhead) {


  }

  @Override
  public void onTernaryEnd(final Token<?> lookhead) {


  }

  @Override
  public void onJoinLeft(final Token<?> lookhead) {


  }

  @Override
  public void onJoinRight(final Token<?> lookhead) {


  }

  @Override
  public void onEq(final Token<?> lookhead) {


  }

  @Override
  public void onMatch(final Token<?> lookhead) {


  }

  @Override
  public void onNeq(final Token<?> lookhead) {


  }

  @Override
  public void onLt(final Token<?> lookhead) {


  }

  @Override
  public void onLe(final Token<?> lookhead) {


  }

  @Override
  public void onGt(final Token<?> lookhead) {


  }

  @Override
  public void onGe(final Token<?> lookhead) {


  }

  @Override
  public void onMod(final Token<?> lookhead) {


  }

  @Override
  public void onNot(final Token<?> lookhead) {


  }

  @Override
  public void onNeg(final Token<?> lookhead) {


  }

  @Override
  public Expression getResult(final boolean unboxObject) {

    return null;
  }

  @Override
  public void onConstant(final Token<?> lookhead) {


  }

  @Override
  public void onMethodName(final Token<?> lookhead) {


  }

  @Override
  public void onMethodParameter(final Token<?> lookhead) {


  }

  @Override
  public void onMethodInvoke(final Token<?> lookhead) {


  }

  @Override
  public void onLambdaDefineStart(final Token<?> lookhead) {
    Boolean newLexicalScope = lookhead.getMeta(Constants.SCOPE_META, false);
    this.infos.push(this.parser.enterScope(newLexicalScope));
  }

  @Override
  public void onLambdaArgument(final Token<?> lookhead, final FunctionParam param) {


  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookhead) {


  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookhead) {
    this.parser.restoreScope(this.infos.pop());
  }

  @Override
  public void onArray(final Token<?> lookhead) {


  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {


  }

  @Override
  public void onArrayIndexEnd(final Token<?> lookhead) {


  }

}
