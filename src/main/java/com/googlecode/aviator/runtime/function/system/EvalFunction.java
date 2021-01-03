package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * eval(script) , eval(script, bindings) and eval(script, bindings, cached) to execute a script with
 * current env or special bindings, default is in caching mode.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class EvalFunction extends AbstractFunction {

  private static final long serialVersionUID = 541446001268353015L;

  public static final EvalFunction INSTANCE = new EvalFunction();

  @Override
  public String getName() {
    return "eval";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    AviatorEvaluatorInstance instance = RuntimeUtils.getInstance(env);
    String script = FunctionUtils.getStringValue(arg1, env);
    return AviatorRuntimeJavaType
        .valueOf(instance.execute(script, env, instance.isCachedExpressionByDefault()));
  }

  @SuppressWarnings("unchecked")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    AviatorEvaluatorInstance instance = RuntimeUtils.getInstance(env);
    String script = FunctionUtils.getStringValue(arg1, env);
    return AviatorRuntimeJavaType.valueOf(instance.execute(script,
        (Map<String, Object>) arg2.getValue(env), instance.isCachedExpressionByDefault()));
  }


  @SuppressWarnings("unchecked")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {
    AviatorEvaluatorInstance instance = RuntimeUtils.getInstance(env);
    String script = FunctionUtils.getStringValue(arg1, env);
    return AviatorRuntimeJavaType.valueOf(instance.execute(script,
        (Map<String, Object>) arg2.getValue(env), FunctionUtils.getBooleanValue(arg3, env)));
  }
}
