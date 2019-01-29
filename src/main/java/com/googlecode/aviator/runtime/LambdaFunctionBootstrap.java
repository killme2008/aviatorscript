package com.googlecode.aviator.runtime;

import java.lang.invoke.MethodHandle;
import java.util.List;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.utils.Env;

/**
 * A lambda function creator.
 *
 * @author dennis
 *
 */
public class LambdaFunctionBootstrap {
  // the generated lambda class name
  private String name;
  // The compiled lambda body expression
  private Expression expression;
  // The method handle to create lambda instance.
  private MethodHandle constructor;
  // The arguments list.
  private List<String> arguments;


  public String getName() {
    return this.name;
  }

  public LambdaFunctionBootstrap(String name, Expression expression, MethodHandle constructor,
      List<String> arguments) {
    super();
    this.name = name;
    this.expression = expression;
    this.constructor = constructor;
    this.arguments = arguments;
  }


  /**
   * Create a lambda function.
   *
   * @param env
   * @return
   */
  public LambdaFunction newInstance(Env env) {
    try {
      return (LambdaFunction) constructor.invoke(arguments, expression, env);
    } catch (ExpressionRuntimeException e) {
      throw e;
    } catch (Throwable t) {
      throw new ExpressionRuntimeException("Fail to create lambda instance.", t);
    }
  }
}
