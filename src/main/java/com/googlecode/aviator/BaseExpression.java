package com.googlecode.aviator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.utils.Env;


/**
 * Base expression
 *
 * @author dennis
 *
 */
public abstract class BaseExpression implements Expression {

  public static final String FUNC_PARAMS_VAR = "__funcs_args__";
  private final List<String> varNames;
  private final List<String> varFullNames;
  private String expression;
  protected AviatorEvaluatorInstance instance;
  private Env compileEnv;
  private Map<Integer, List<FunctionArgument>> funcsArgs = Collections.emptyMap();


  public BaseExpression(final AviatorEvaluatorInstance instance, final List<String> varNames) {
    super();
    this.varFullNames = varNames;
    this.instance = instance;
    LinkedHashSet<String> tmp = new LinkedHashSet<String>(varNames.size());
    // process nested names
    for (String name : varNames) {
      if (name.contains(".")) {
        name = name.substring(0, name.indexOf("."));
      }
      tmp.add(name);
    }
    this.varNames = new ArrayList<String>(tmp);
  }


  public void setFuncsArgs(final Map<Integer, List<FunctionArgument>> funcsArgs) {
    if (funcsArgs != null) {
      this.funcsArgs = Collections.unmodifiableMap(funcsArgs);
    }
  }


  public Env getCompileEnv() {
    return this.compileEnv;
  }


  public void setCompileEnv(final Env compileEnv) {
    this.compileEnv = compileEnv;
  }


  /**
   * Returns the expression string when turn on {@link Options#TRACE_EVAL} option, else returns
   * null.
   *
   * @return expression in string.
   */
  public String getExpression() {
    return this.expression;
  }

  public void setExpression(final String expression) {
    this.expression = expression;
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#execute()
   */
  @Override
  public Object execute() {
    return this.execute(null);
  }


  @Override
  public List<String> getVariableFullNames() {
    return this.varFullNames;
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#getVariableNames()
   */
  @Override
  public List<String> getVariableNames() {
    return this.varNames;
  }

  protected Env newEnv(final Map<String, Object> map, final boolean direct) {
    Env env;
    if (direct) {
      env = new Env(map, map == Collections.EMPTY_MAP ? new HashMap<String, Object>() : map);
    } else {
      env = new Env(map);
    }
    env.setInstance(this.instance);
    return env;
  }

  protected Env genTopEnv(final Map<String, Object> map) {
    Env env =
        newEnv(map, this.instance.getOptionValue(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY).bool);
    if (this.compileEnv != null && !this.compileEnv.isEmpty()) {
      env.putAll(this.compileEnv);
    }
    if (!this.funcsArgs.isEmpty()) {
      env.put(FUNC_PARAMS_VAR, this.funcsArgs);
    }
    return env;
  }

  protected Env newEnv(final Map<String, Object> map) {
    return newEnv(map, false);
  }

}
