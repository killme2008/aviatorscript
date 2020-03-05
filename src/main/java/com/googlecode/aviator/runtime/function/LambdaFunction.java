package com.googlecode.aviator.runtime.function;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

/**
 * All lamabda function base class
 *
 * @author dennis
 *
 */
public abstract class LambdaFunction extends AbstractFunction {

  // the arguments.
  protected List<String> arguments;

  // the compiled lambda body expression
  protected Expression expression;

  // closure context
  protected Env context;

  public LambdaFunction(final List<String> arguments, final Expression expression,
      final Env context) {
    super();
    this.arguments = arguments;
    this.context = context;
    this.expression = expression;
  }

  protected Map<String, Object> newEnv(final Map<String, Object> parentEnv,
      final AviatorObject... args) {
    Env env = new Env(new Env(parentEnv, this.context));
    env.setInstance(this.context.getInstance());
    int i = 0;
    for (String name : this.arguments) {
      env.override(name, args[i++].getValue(parentEnv));
    }

    return env;
  }
}
