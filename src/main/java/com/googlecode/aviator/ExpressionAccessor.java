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
package com.googlecode.aviator;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.utils.Env;

/**
 * Base expression default methods accessor
 * 
 * @author dennis
 *
 */
public class ExpressionAccessor {

  public static void setSourceFile(final BaseExpression exp, String sourceFile) {
    exp.setSourceFile(sourceFile);
  }

  public static void setInstance(final BaseExpression exp, AviatorEvaluatorInstance instance) {
    exp.setInstance(instance);
  }

  public static void setCompileEnv(final BaseExpression exp, final Env compileEnv) {
    exp.setCompileEnv(compileEnv);
  }

  public static void setExpression(final BaseExpression exp, final String expression) {
    exp.setExpression(expression);
  }

  public static void setFuncsArgs(final BaseExpression exp,
      final Map<Integer, List<FunctionArgument>> funcsArgs) {
    exp.setFuncsArgs(funcsArgs);
  }

  public static void setLambdaBootstraps(final BaseExpression exp,
      final Map<String, LambdaFunctionBootstrap> lambdaBootstraps) {
    exp.setLambdaBootstraps(lambdaBootstraps);
  }

  public static void setFunctionNames(final BaseExpression exp, List<String> functionNames) {
    exp.setFunctionNames(functionNames);
  }
}
