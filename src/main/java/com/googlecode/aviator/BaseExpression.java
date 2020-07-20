package com.googlecode.aviator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.FutureTask;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.type.string.StringSegment;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;


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
  protected SymbolTable symbolTable;
  // cached compiled string segments for string interpolation.
  private final ConcurrentHashMap<String, FutureTask<List<StringSegment>>> stringSegs =
      new ConcurrentHashMap<String, FutureTask<List<StringSegment>>>();

  public BaseExpression(final AviatorEvaluatorInstance instance, final List<String> varNames,
      final SymbolTable symbolTable) {
    super();
    this.symbolTable = symbolTable;
    this.varFullNames = varNames;
    this.instance = instance;
    LinkedHashSet<String> tmp = new LinkedHashSet<>(varNames.size());
    // process nested names
    for (String name : varNames) {
      if (name.contains(".")) {
        name = name.substring(0, name.indexOf("."));
      }
      tmp.add(name);
    }
    this.varNames = new ArrayList<String>(tmp);
  }

  public List<StringSegment> getStringSegements(final String lexeme) {
    FutureTask<List<StringSegment>> task = this.stringSegs.get(lexeme);
    if (task == null) {
      task = new FutureTask<>(new Callable<List<StringSegment>>() {
        @Override
        public List<StringSegment> call() throws Exception {
          final List<StringSegment> compiledSegs =
              BaseExpression.this.instance.compileStringSegments(lexeme);
          return compiledSegs;
        }
      });

      FutureTask<List<StringSegment>> existsTask = this.stringSegs.putIfAbsent(lexeme, task);
      if (existsTask != null) {
        task = existsTask;
      } else {
        task.run(); // first run
      }
    }

    try {
      return task.get();
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

  private class SymbolHashMap extends HashMap<String, Object> {

    private static final long serialVersionUID = 5951510458689965590L;

    public SymbolHashMap(final int initialCapacity) {
      super(initialCapacity);
    }

    @Override
    public Object put(String key, final Object value) {
      Variable var = null;
      if (BaseExpression.this.symbolTable != null
          && (var = BaseExpression.this.symbolTable.getVariable(key)) != null) {
        key = var.getLexeme();
      }
      return super.put(key, value);
    }

  }

  @Override
  public Map<String, Object> newEnv(final Object... args) {
    if (args != null && args.length % 2 != 0) {
      throw new IllegalArgumentException("Expect arguments number is even.");
    }
    Map<String, Object> env = new SymbolHashMap(args != null ? args.length : 10);
    if (args != null) {
      for (int i = 0; i < args.length; i += 2) {
        String key = (String) args[i];
        env.put(key, args[i + 1]);
      }
    }
    return env;
  }

  public abstract Object executeDirectly(final Map<String, Object> env);

  @Override
  public Object execute(Map<String, Object> map) {
    if (map == null) {
      map = Collections.emptyMap();
    }
    Env env = genTopEnv(map);
    return executeDirectly(env);
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


  @Override
  public String addSymbol(final String name) {
    if (this.symbolTable != null) {
      return this.symbolTable.reserve(name).getLexeme();
    } else {
      return name;
    }
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
    if (map instanceof Env) {
      ((Env) map).setInstance(this.instance);
    }
    Env env =
        newEnv(map, this.instance.getOptionValue(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY).bool);

    if (this.compileEnv != null && !this.compileEnv.isEmpty()) {
      env.putAll(this.compileEnv);
    }
    if (!this.funcsArgs.isEmpty()) {
      env.override(FUNC_PARAMS_VAR, this.funcsArgs);
    }
    env.override(Constants.EXP_VAR, this);
    return env;
  }

  protected Env newEnv(final Map<String, Object> map) {
    return newEnv(map, false);
  }

}
