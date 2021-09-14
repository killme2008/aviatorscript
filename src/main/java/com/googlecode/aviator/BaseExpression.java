package com.googlecode.aviator;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.HashSet;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.FutureTask;
import com.googlecode.aviator.AviatorEvaluatorInstance.StringSegments;
import com.googlecode.aviator.exception.ExpressionNotFoundException;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.function.LambdaFunction;
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
  protected List<String> varNames;
  protected List<String> varFullNames;
  private final List<VariableMeta> vars;
  private String expression;
  protected AviatorEvaluatorInstance instance;
  private Env compileEnv;
  private Map<Integer, List<FunctionArgument>> funcsArgs = Collections.emptyMap();
  protected SymbolTable symbolTable;
  // cached compiled string segments for string interpolation.
  private final ConcurrentHashMap<String, FutureTask<StringSegments>> stringSegs =
      new ConcurrentHashMap<String, FutureTask<StringSegments>>();

  protected String sourceFile;
  protected Map<String, LambdaFunctionBootstrap> lambdaBootstraps;


  @Override
  public String getSourceFile() {
    return this.sourceFile;
  }

  public void setSourceFile(final String sourceFile) {
    this.sourceFile = sourceFile;
  }

  public BaseExpression(final AviatorEvaluatorInstance instance, final List<VariableMeta> vars,
      final SymbolTable symbolTable) {
    super();
    this.vars = vars;
    this.symbolTable = symbolTable;
    this.instance = instance;
  }

  private void populateNames() {
    if (this.varNames == null) {
      if (this.varFullNames == null) {
        populateFullNames();
      }

      List<String> newVarNames = new ArrayList<>(this.varFullNames.size());

      Set<String> nameSet = new HashSet<>();

      Set<String> parentInitNames = new HashSet<>();
      for (VariableMeta m : this.vars) {
        if (m.isInit() && !m.getName().contains(".") && m.getFirstIndex() >= 0) {
          parentInitNames.add(m.getName());
        }
      }

      for (String fName : this.varFullNames) {
        String[] tmps = Constants.SPLIT_PAT.split(fName);
        String sName = tmps[0];
        if (!nameSet.contains(sName) && !parentInitNames.contains(sName)) {
          newVarNames.add(sName);
          nameSet.add(sName);
        }
      }
      this.varNames = newVarNames;
    }
  }

  protected void afterPopulateFullNames(final Map<String, VariableMeta> fullNames,
      final Set<String> parentVars) {
    if (this.lambdaBootstraps != null && !this.lambdaBootstraps.isEmpty()) {
      for (LambdaFunctionBootstrap bootstrap : this.lambdaBootstraps.values()) {
        for (VariableMeta meta : bootstrap.getClosureOverFullVarNames()) {
          VariableMeta existsMeta = fullNames.get(meta.getName());
          if (existsMeta == null) {
            if (!parentVars.contains(meta.getName())) {
              fullNames.put(meta.getName(), meta);
            }
          } else {
            // Appear first, update the meta
            if (existsMeta.getFirstIndex() > meta.getFirstIndex()) {
              fullNames.put(meta.getName(), meta);
            }
          }
        }
      }
    }
  }

  private void populateFullNames() {
    if (this.varFullNames == null) {
      Map<String, VariableMeta> fullNames = getFullNameMetas();


      final ArrayList<VariableMeta> metas = new ArrayList<>(fullNames.values());
      Collections.sort(metas, new Comparator<VariableMeta>() {

        @Override
        public int compare(final VariableMeta o1, final VariableMeta o2) {
          return Integer.compare(o1.getFirstIndex(), o2.getFirstIndex());
        }

      });

      List<String> newFullNames = new ArrayList<>(fullNames.size());
      for (VariableMeta meta : metas) {
        newFullNames.add(meta.getName());
      }


      this.varFullNames = newFullNames;
    }
  }

  public Map<String, VariableMeta> getFullNameMetas() {
    Map<String, VariableMeta> fullNames = new LinkedHashMap<>(this.vars.size());
    Set<String> parentVars = new HashSet<>(this.vars.size());
    for (VariableMeta m : this.vars) {
      if (!m.isInit() && m.getFirstIndex() >= 0) {
        fullNames.put(m.getName(), m);
      }
      parentVars.add(m.getName());
    }
    afterPopulateFullNames(fullNames, parentVars);
    return fullNames;
  }

  public StringSegments getStringSegements(final String lexeme, final int lineNo) {
    FutureTask<StringSegments> task = this.stringSegs.get(lexeme);
    if (task == null) {
      task = new FutureTask<>(new Callable<StringSegments>() {
        @Override
        public StringSegments call() throws Exception {
          final StringSegments compiledSegs = BaseExpression.this.instance
              .compileStringSegments(lexeme, BaseExpression.this.sourceFile, lineNo);
          return compiledSegs;
        }
      });

      FutureTask<StringSegments> existsTask = this.stringSegs.putIfAbsent(lexeme, task);
      if (existsTask != null) {
        task = existsTask;
      } else {
        task.run(); // first run
      }
    }

    try {
      return task.get();
    } catch (ExecutionException t) {
      throw Reflector.sneakyThrow(t.getCause());
    } catch (InterruptedException e) {
      Thread.currentThread().interrupt();
      throw Reflector.sneakyThrow(e);
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
    EnvProcessor envProcessor = this.instance.getEnvProcessor();
    if (envProcessor != null) {
      envProcessor.beforeExecute(env, this);
    }
    try {
      return executeDirectly(env);
    } finally {
      if (envProcessor != null) {
        envProcessor.afterExecute(env, this);
      }
    }
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
    this.compileEnv.setExpression(this);
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
    populateFullNames();
    return this.varFullNames;
  }

  public List<VariableMeta> getVars() {
    return this.vars;
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#getVariableNames()
   */
  @Override
  public List<String> getVariableNames() {
    populateNames();
    return this.varNames;
  }

  protected Env newEnv(final Map<String, Object> map, final boolean direct) {
    Env env;
    if (direct) {
      env = new Env(map, map == Collections.EMPTY_MAP ? new HashMap<String, Object>() : map);
    } else {
      env = new Env(map);
    }
    env.configure(this.instance, this);
    return env;
  }

  protected Env genTopEnv(final Map<String, Object> map) {
    if (map instanceof Env) {
      ((Env) map).configure(this.instance, this);
    }
    Env env =
        newEnv(map, this.instance.getOptionValue(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY).bool);

    if (this.compileEnv != null && !this.compileEnv.isEmpty()) {
      env.putAll(this.compileEnv);
    }
    if (!this.funcsArgs.isEmpty()) {
      env.override(FUNC_PARAMS_VAR, this.funcsArgs);
    }
    return env;
  }

  protected Env newEnv(final Map<String, Object> map) {
    return newEnv(map, false);
  }

  public Map<String, LambdaFunctionBootstrap> getLambdaBootstraps() {
    return this.lambdaBootstraps;
  }

  public void setLambdaBootstraps(final Map<String, LambdaFunctionBootstrap> lambdaBootstraps) {
    this.lambdaBootstraps = lambdaBootstraps;
  }

  public LambdaFunction newLambda(final Env env, final String name) {
    LambdaFunctionBootstrap bootstrap = this.lambdaBootstraps.get(name);
    if (bootstrap == null) {
      throw new ExpressionNotFoundException("Lambda " + name + " not found");
    }
    return bootstrap.newInstance(env);
  }

}
