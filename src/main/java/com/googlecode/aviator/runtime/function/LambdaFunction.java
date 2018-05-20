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

  protected List<String> arguments;

  protected Expression expression;


  public LambdaFunction(List<String> arguments, Expression expression) {
    super();
    this.arguments = arguments;
    this.expression = expression;
  }

  protected Map<String, Object> getEnv(Map<String, Object> parentEnv, AviatorObject... args) {
    Env env = new Env(parentEnv);
    int i = 0;
    for (String name : arguments) {
      env.put(name, args[i++].getValue(env));
    }
    return env;
  }
}
