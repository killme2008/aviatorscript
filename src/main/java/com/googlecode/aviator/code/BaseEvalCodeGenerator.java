package com.googlecode.aviator.code;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.code.asm.ASMCodeGenerator.MethodMetaData;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.Parser;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.utils.Env;

public abstract class BaseEvalCodeGenerator implements EvalCodeGenerator {

  protected final AviatorEvaluatorInstance instance;
  protected Map<String, VariableMeta> variables = Collections.emptyMap();
  protected final String sourceFile;
  protected LambdaGenerator lambdaGenerator;
  protected final AviatorClassLoader classLoader;
  protected Parser parser;
  protected SymbolTable symbolTable;
  /**
   * parent code generator when compiling lambda.
   */
  protected CodeGenerator parentCodeGenerator;
  /**
   * Compiled lambda functions.
   */
  protected Map<String, LambdaFunctionBootstrap> lambdaBootstraps;
  protected final ArrayDeque<MethodMetaData> methodMetaDataStack = new ArrayDeque<>();
  /**
   * function params info.
   */
  protected Map<Integer/* internal function id */, List<FunctionArgument>> funcsArgs;
  private int funcInvocationId = 0;
  /**
   * Compile environment only has the *instance*.
   */
  protected final Env compileEnv;

  protected Map<String, Integer/* counter */> methodTokens = Collections.emptyMap();

  protected Map<Integer/* internal function id */, List<FunctionArgument>> getFuncsArgs() {
    if (this.funcsArgs == null) {
      this.funcsArgs = new HashMap<>();
    }
    return this.funcsArgs;
  }

  protected int getNextFuncInvocationId() {
    return this.funcInvocationId++;
  }

  @Override
  public void initMethods(Map<String, Integer> methods) {
    this.methodTokens = methods;
  }

  @Override
  public void setParser(final Parser parser) {
    this.parser = parser;
    this.symbolTable = this.parser.getSymbolTable();
  }

  @Override
  public void setLambdaBootstraps(final Map<String, LambdaFunctionBootstrap> lambdaBootstraps) {
    this.lambdaBootstraps = lambdaBootstraps;
  }

  @Override
  public AviatorClassLoader getClassLoader() {
    return this.classLoader;
  }

  public BaseEvalCodeGenerator(final AviatorEvaluatorInstance instance, final String sourceFile,
      final AviatorClassLoader classLoader) {
    super();
    this.instance = instance;
    this.compileEnv = new Env();
    this.compileEnv.setInstance(this.instance);
    this.sourceFile = sourceFile;
    this.classLoader = classLoader;
  }

}
