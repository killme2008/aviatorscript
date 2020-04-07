package com.googlecode.aviator.runtime.function;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.BaseExpression;
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


  private static final long serialVersionUID = -4388339706945053452L;

  // the arguments.
  protected List<String> arguments;

  // the compiled lambda body expression
  protected BaseExpression expression;

  // closure context
  protected Env context;

  // whether to inherit parent env
  private boolean inheritEnv = false;

  public void setInheritEnv(final boolean inheritEnv) {
    this.inheritEnv = inheritEnv;
  }

  public LambdaFunction(final List<String> arguments, final Expression expression,
      final Env context) {
    super();
    this.arguments = arguments;
    this.context = context;
    this.expression = (BaseExpression) expression;
  }

  protected Map<String, Object> newEnv(final Map<String, Object> parentEnv,
      final AviatorObject... args) {
    Env env = null;
    if (!this.inheritEnv) {
      final Env contextEnv = new Env(parentEnv, this.context);
      contextEnv.setInstance(this.context.getInstance());
      env = new Env(contextEnv);
      env.setInstance(this.context.getInstance());
    } else {
      assert (parentEnv == this.context);
      env = (Env) parentEnv;
    }
    int i = 0;
    for (String name : this.arguments) {
      env.override(name, args[i++].getValue(parentEnv));
    }

    return env;
  }
}
