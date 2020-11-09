package com.googlecode.aviator.runtime.function;

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.BaseExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.runtime.FunctionParam;
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
  protected List<FunctionParam> params;

  // the compiled lambda body expression
  protected BaseExpression expression;

  // closure context
  protected Env context;

  // whether to inherit parent env
  private boolean inheritEnv = false;

  private final boolean isVariadic;

  public void setInheritEnv(final boolean inheritEnv) {
    this.inheritEnv = inheritEnv;
  }

  public LambdaFunction(final List<FunctionParam> params, final Expression expression,
      final Env context) {
    super();
    this.params = params;
    this.context = context;
    this.expression = (BaseExpression) expression;
    if (!this.params.isEmpty()) {
      this.isVariadic = this.params.get(this.params.size() - 1).isVariadic();
    } else {
      this.isVariadic = false;
    }
  }

  public int getArity() {
    return this.params.size();
  }

  public boolean isVariadic() {
    return this.isVariadic;
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
    for (FunctionParam param : this.params) {
      final AviatorObject arg = args[i++];
      Object value = arg.getValue(parentEnv);
      if (value == null && arg.getAviatorType() == AviatorType.JavaType
          && !parentEnv.containsKey(((AviatorJavaType) arg).getName())) {
        value = RuntimeUtils.getInstance(parentEnv).getFunction(((AviatorJavaType) arg).getName());
      }
      env.override(param.getName(), value);
    }

    return env;
  }
}
