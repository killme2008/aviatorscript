package com.googlecode.aviator.code;

import java.util.ArrayDeque;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.InterpretExpression;
import com.googlecode.aviator.code.asm.ASMCodeGenerator.MethodMetaData;
import com.googlecode.aviator.code.interpreter.IR;
import com.googlecode.aviator.code.interpreter.ir.LoadIR;
import com.googlecode.aviator.code.interpreter.ir.NewLambdaIR;
import com.googlecode.aviator.code.interpreter.ir.OperatorIR;
import com.googlecode.aviator.code.interpreter.ir.SendIR;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.runtime.FunctionParam;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.utils.Constants;

/**
 * Generate expression based on IR for interpreting.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class InterpretCodeGenerator implements CodeGenerator {
  private final List<IR> instruments = new ArrayList<>();
  private final AviatorEvaluatorInstance instance;


  private final String sourceFile;

  private LambdaGenerator lambdaGenerator;

  private final AviatorClassLoader classLoader;

  private Parser parser;

  /**
   * parent code generator when compiling lambda.
   */
  private CodeGenerator parentCodeGenerator;

  /**
   * Compiled lambda functions.
   */
  private Map<String, LambdaFunctionBootstrap> lambdaBootstraps;

  private final ArrayDeque<MethodMetaData> methodMetaDataStack = new ArrayDeque<>();

  public InterpretCodeGenerator(final AviatorEvaluatorInstance instance, final String sourceFile,
      final AviatorClassLoader classLoader) {
    super();
    this.instance = instance;
    this.sourceFile = sourceFile;
    this.classLoader = classLoader;
  }

  @Override
  public void onAssignment(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.ASSIGN);
  }

  @Override
  public void setParser(final Parser parser) {
    this.parser = parser;
  }

  @Override
  public void onShiftRight(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.SHIFT_RIGHT);
  }

  @Override
  public void onShiftLeft(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.SHIFT_LEFT);
  }

  @Override
  public void onUnsignedShiftRight(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.UNSIGNED_SHIFT_RIGHT);
  }

  @Override
  public void onBitOr(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.BIT_OR);
  }

  @Override
  public void onBitAnd(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.BIT_AND);
  }

  @Override
  public void onBitXor(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.BIT_XOR);
  }

  @Override
  public void onBitNot(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.BIT_NOT);
  }

  @Override
  public void onAdd(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.ADD);
  }

  @Override
  public void onSub(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.SUB);

  }

  @Override
  public void onMult(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.MULT);
  }

  @Override
  public void onExponent(final Token<?> loohead) {
    this.instruments.add(OperatorIR.EXP);
  }

  @Override
  public void onDiv(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.DIV);

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
    this.instruments.add(OperatorIR.EQ);
  }

  @Override
  public void onMatch(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.MATCH);
  }

  @Override
  public void onNeq(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.NE);
  }

  @Override
  public void onLt(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.LT);
  }

  @Override
  public void onLe(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.LE);
  }

  @Override
  public void onGt(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.GT);
  }

  @Override
  public void onGe(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.GE);
  }

  @Override
  public void onMod(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.MOD);
  }

  @Override
  public void onNot(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.NOT);
  }

  @Override
  public void onNeg(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.NEG);
  }

  @Override
  public Expression getResult(final boolean unboxObject) {
    final InterpretExpression exp =
        new InterpretExpression(this.instance, Collections.EMPTY_LIST, null, this.instruments);
    exp.setLambdaBootstraps(this.lambdaBootstraps);

    return exp;
  }

  @Override
  public void onConstant(final Token<?> lookhead) {
    this.instruments.add(new LoadIR(lookhead));
  }

  @Override
  public void onMethodName(final Token<?> lookhead) {
    this.methodMetaDataStack.push(new MethodMetaData(lookhead.getLexeme()));
  }

  @Override
  public void onMethodParameter(final Token<?> lookhead) {
    MethodMetaData currentMethodMetaData = this.methodMetaDataStack.peek();
    currentMethodMetaData.parameterCount++;
  }

  @Override
  public void onMethodInvoke(final Token<?> lookhead) {
    final MethodMetaData methodMetaData = this.methodMetaDataStack.pop();
    this.instruments.add(new SendIR(methodMetaData.methodName, methodMetaData.parameterCount));
  }

  @Override
  public void onLambdaDefineStart(final Token<?> lookhead) {
    if (this.lambdaGenerator == null) {
      Boolean newLexicalScope = lookhead.getMeta(Constants.SCOPE_META, false);
      Boolean inheritEnv = lookhead.getMeta(Constants.INHERIT_ENV_META, false);
      // TODO cache?
      this.lambdaGenerator = new LambdaGenerator(this.instance, this, this.parser, this.classLoader,
          this.sourceFile, newLexicalScope, inheritEnv);
      this.lambdaGenerator.setScopeInfo(this.parser.enterScope(newLexicalScope));
    } else {
      throw new CompileExpressionErrorException("Compile lambda error");
    }

  }

  @Override
  public void onLambdaArgument(final Token<?> lookhead, final FunctionParam param) {
    this.lambdaGenerator.addParam(param);
  }

  @Override
  public void onLambdaBodyStart(final Token<?> lookhead) {
    this.parentCodeGenerator = this.parser.getCodeGenerator();
    this.parser.setCodeGenerator(this.lambdaGenerator);
  }

  @Override
  public void onLambdaBodyEnd(final Token<?> lookhead) {
    // this.lambdaGenerator.compileCallMethod();
    LambdaFunctionBootstrap bootstrap = this.lambdaGenerator.getLmabdaBootstrap();
    if (this.lambdaBootstraps == null) {
      // keep in order
      this.lambdaBootstraps = new LinkedHashMap<String, LambdaFunctionBootstrap>();
    }
    this.lambdaBootstraps.put(bootstrap.getName(), bootstrap);
    this.instruments.add(new NewLambdaIR(bootstrap.getName()));
    this.parser.restoreScope(this.lambdaGenerator.getScopeInfo());
    this.lambdaGenerator = null;
    this.parser.setCodeGenerator(this.parentCodeGenerator);
  }

  @Override
  public void onArray(final Token<?> lookhead) {
    this.instruments.add(new LoadIR(lookhead));
  }

  @Override
  public void onArrayIndexStart(final Token<?> token) {
    // TODO
  }

  @Override
  public void onArrayIndexEnd(final Token<?> lookhead) {
    this.instruments.add(OperatorIR.INDEX);
  }

}
