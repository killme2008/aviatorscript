package com.googlecode.aviator.runtime;

import java.util.Collection;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.BaseExpression;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.parser.VariableMeta;
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
  private final BaseExpression expression;
  // The method handle to create lambda instance.
  // private final MethodHandle constructor;
  // The arguments list.
  private final List<FunctionParam> params;
  private final boolean inheritEnv;

  private final ThreadLocal<LambdaFunction> fnLocal = new ThreadLocal<>();


  public String getName() {
    return this.name;
  }

  public LambdaFunctionBootstrap(final String name, final Expression expression,
      final List<FunctionParam> arguments, final boolean inheritEnv) {
    super();
    this.name = name;
    this.expression = (BaseExpression) expression;
    // this.constructor = constructor;
    this.params = arguments;
    this.inheritEnv = inheritEnv;
  }


  public Collection<VariableMeta> getClosureOverFullVarNames() {
    Map<String, VariableMeta> fullNames = this.expression.getFullNameMetas();

    for (FunctionParam param : this.params) {
      fullNames.remove(param.getName());
    }

    Iterator<Map.Entry<String, VariableMeta>> it = fullNames.entrySet().iterator();
    while (it.hasNext()) {
      Map.Entry<String, VariableMeta> fullName = it.next();
      for (FunctionParam param : this.params) {
        if (fullName.getKey().startsWith(param.getName() + ".")) {
          it.remove();
          break;
        }
      }
    }

    return fullNames.values();
  }

  public Expression getExpression() {
    return this.expression;
  }

  /**
   * Create a lambda function.
   *
   * @param env
   * @return
   */
  public LambdaFunction newInstance(final Env env) {
    LambdaFunction fn = null;
    if (this.inheritEnv && (fn = this.fnLocal.get()) != null) {
      fn.setContext(env);
      return fn;
    }

    // try {
    fn = new LambdaFunction(this.name, this.params, this.expression, env);
    fn.setInheritEnv(this.inheritEnv);
    if (this.inheritEnv) {
      this.fnLocal.set(fn);
    }
    return fn;
    // final LambdaFunction fn =
    // (LambdaFunction) this.constructor.invoke(this.params, this.expression, env);

    // } catch (ExpressionRuntimeException e) {
    // throw e;
    // } catch (Throwable t) {
    // throw new ExpressionRuntimeException("Fail to create lambda instance.", t);
    // }
  }
}
