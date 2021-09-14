package com.googlecode.aviator.code;

import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;

public interface EvalCodeGenerator extends CodeGenerator {
  void start();

  void initVariables(final Map<String, VariableMeta/* counter */> vars);

  void initConstants(final Set<Token<?>> constants);

  void initMethods(final Map<String, Integer/* counter */> methods);

  void setLambdaBootstraps(final Map<String, LambdaFunctionBootstrap> lambdaBootstraps);

  AviatorClassLoader getClassLoader();

  void genNewLambdaCode(final LambdaFunctionBootstrap bootstrap);
}
