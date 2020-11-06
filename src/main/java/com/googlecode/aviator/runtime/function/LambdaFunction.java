package com.googlecode.aviator.runtime.function;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.BaseExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;
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

  public int getArity() {
    return this.arguments.size();
  }

  protected Map<String, Object> newEnv(final Map<String, Object> parentEnv,
      final AviatorObject... args) {
    Env env = null;
    if (!this.inheritEnv) {
      final Env contextEnv = new Env(parentEnv, this.context);
      contextEnv.configure(this.context.getInstance(), this.expression);
      env = new Env(contextEnv);
      env.configure(this.context.getInstance(), this.expression);
    } else {
      assert (parentEnv == this.context);
      env = (Env) parentEnv;
    }
    int i = 0;
    for (String name : this.arguments) {
      final AviatorObject arg = args[i++];
      Object value = arg.getValue(parentEnv);
      if (value == null && arg.getAviatorType() == AviatorType.JavaType
          && !parentEnv.containsKey(((AviatorJavaType) arg).getName())) {
        value = RuntimeUtils.getInstance(parentEnv).getFunction(((AviatorJavaType) arg).getName());
      }
      env.override(name, value);
    }

    return env;
  }
}
