package com.googlecode.aviator.runtime;

import java.io.Serializable;
import java.lang.ref.Reference;
import java.lang.ref.SoftReference;
import java.lang.ref.WeakReference;
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
public class LambdaFunctionBootstrap implements Comparable<LambdaFunctionBootstrap>, Serializable {
  private static final long serialVersionUID = -8884911908304713609L;
  // the generated lambda class name
  private final String name;
  // The compiled lambda body expression
  private final BaseExpression expression;
  // The method handle to create lambda instance.
  // private final MethodHandle constructor;
  // The arguments list.
  private final List<FunctionParam> params;
  private final boolean inheritEnv;

  private transient ThreadLocal<Reference<LambdaFunction>> fnLocal = new ThreadLocal<>();


  @Override
  public int compareTo(final LambdaFunctionBootstrap o) {
    return this.name.compareTo(o.name);
  }

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
    Reference<LambdaFunction> ref = null;
    if (this.fnLocal == null) {
      this.fnLocal = new ThreadLocal<Reference<LambdaFunction>>();
    }
    if (this.inheritEnv && (ref = this.fnLocal.get()) != null) {
      LambdaFunction fn = ref.get();
      if (fn != null) {
        fn.setContext(env);
        return fn;
      } else {
        this.fnLocal.remove();
      }
    }

    LambdaFunction fn = new LambdaFunction(this.name, this.params, this.expression, env);
    fn.setInheritEnv(this.inheritEnv);
    if (this.inheritEnv) {
      this.fnLocal.set(new SoftReference<>(fn));
    }
    return fn;
  }
}
