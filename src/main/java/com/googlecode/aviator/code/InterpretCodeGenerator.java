package com.googlecode.aviator.code;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.InterpreExpression;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.ir.LoadIR;
import com.googlecode.aviator.code.interpreter.ir.OperatorIR;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.runtime.FunctionParam;

/**
 * Generate expression based on IR for interpreting.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class InterpretCodeGenerator implements CodeGenerator {
  private final List<IR> instruments = new ArrayList<>();
  private final AviatorEvaluatorInstance instance;

  private final SymbolTable symbolTable;



  public InterpretCodeGenerator(final AviatorEvaluatorInstance instance,
      final SymbolTable symbolTable) {
    super();
    this.instance = instance;
    this.symbolTable = symbolTable;
  }

  @Override
  public void onAssignment(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void setParser(final Parser parser) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onShiftRight(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onShiftLeft(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onUnsignedShiftRight(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onBitOr(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onBitAnd(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onBitXor(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onBitNot(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onAdd(final Token<?> lookhead) {
    this.instruments.add(new OperatorIR(OperatorType.ADD));
  }

  @Override
  public void onSub(final Token<?> lookhead) {
    this.instruments.add(new OperatorIR(OperatorType.SUB));

  }

  @Override
  public void onMult(final Token<?> lookhead) {
    this.instruments.add(new OperatorIR(OperatorType.MULT));
  }

  @Override
  public void onExponent(final Token<?> loohead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onDiv(final Token<?> lookhead) {
    this.instruments.add(new OperatorIR(OperatorType.DIV));

  }

  @Override
  public void onAndLeft(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onAndRight(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onTernaryBoolean(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onTernaryLeft(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onTernaryRight(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onTernaryEnd(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onJoinLeft(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onJoinRight(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onEq(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onMatch(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onNeq(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onLt(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onLe(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onGt(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onGe(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onMod(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onNot(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onNeg(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public Expression getResult(final boolean unboxObject) {
    return new InterpreExpression(this.instance, Collections.EMPTY_LIST, this.symbolTable,
        this.instruments);
  }

  @Override
  public void onConstant(final Token<?> lookhead) {
    this.instruments.add(new LoadIR(lookhead));
  }

  @Override
  public void onMethodName(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onMethodParameter(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onMethodInvoke(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onLambdaDefineStart(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onLambdaArgument(final Token<?> lookhead, final FunctionParam param) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onArray(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {
    // TODO Auto-generated method stub

  }

  @Override
  public void onArrayIndexEnd(final Token<?> lookhead) {
    // TODO Auto-generated method stub

  }

}
