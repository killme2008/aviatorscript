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
  private final String name;
  // The compiled lambda body expression
  private final Expression expression;
  // The method handle to create lambda instance.
  private final MethodHandle constructor;
  // The arguments list.
  private final List<FunctionParam> params;
  private final boolean inheritEnv;


  public String getName() {
    return this.name;
  }

  public LambdaFunctionBootstrap(final String name, final Expression expression,
      final MethodHandle constructor, final List<FunctionParam> arguments,
      final boolean inheritEnv) {
    super();
    this.name = name;
    this.expression = expression;
    this.constructor = constructor;
    this.params = arguments;
    this.inheritEnv = inheritEnv;
  }


  /**
   * Create a lambda function.
   *
   * @param env
   * @return
   */
  public LambdaFunction newInstance(final Env env) {
    try {
      final LambdaFunction fn =
          (LambdaFunction) this.constructor.invoke(this.params, this.expression, env);
      fn.setInheritEnv(this.inheritEnv);
      return fn;
    } catch (ExpressionRuntimeException e) {
      throw e;
    } catch (Throwable t) {
      throw new ExpressionRuntimeException("Fail to create lambda instance.", t);
    }
  }
}
