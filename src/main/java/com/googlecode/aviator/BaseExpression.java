package com.googlecode.aviator;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
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
import com.googlecode.aviator.parser.CompileTypes;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.FunctionArgument;
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.Utils;

/**
 * Base expression
 *
 * @author dennis
 *
 */
public abstract class BaseExpression implements Expression {

  private static final long serialVersionUID = 2819544277750883372L;

  public static final String FUNC_PARAMS_VAR = "__funcs_args__";
  protected transient List<String> varNames;
  protected transient List<String> varFullNames;
  private List<VariableMeta> vars;
  private String expression;
  protected transient AviatorEvaluatorInstance instance;
  private Env compileEnv;
  private Map<Integer, List<FunctionArgument>> funcsArgs = Collections.emptyMap();
  protected SymbolTable symbolTable;
  // cached compiled string segments for string interpolation.
  private transient ConcurrentHashMap<String, FutureTask<StringSegments>> stringSegs =
      new ConcurrentHashMap<String, FutureTask<StringSegments>>();
  // The function name list in expression
  private List<String> functionNames = Collections.emptyList();

  // Already filtered function names, try to remove the runtime defined functions.
  private transient List<String> filteredFunctionNames = null;


  protected String sourceFile;
  protected Map<String, LambdaFunctionBootstrap> lambdaBootstraps;

  @Override
  public String getSourceFile() {
    return this.sourceFile;
  }

  protected void setSourceFile(final String sourceFile) {
    this.sourceFile = sourceFile;
  }


  protected void setInstance(AviatorEvaluatorInstance instance) {
    this.instance = instance;
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
          final String name = meta.getName();

          VariableMeta existsMeta = fullNames.get(name);
          if (existsMeta == null) {
            String[] tmps = Constants.SPLIT_PAT.split(name);
            if (!parentVars.contains(tmps[0])) {
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
    Set<String> definedVars = new HashSet<>();

    for (VariableMeta m : this.vars) {
      final String name = m.getName();
      String[] tmps = Constants.SPLIT_PAT.split(name);
      if (!m.isInit() && m.getType() != CompileTypes.Class && !definedVars.contains(tmps[0])
          && !definedVars.contains(name) && m.getFirstIndex() >= 0) {
        fullNames.put(name, m);
      } else if (m.getFirstIndex() >= 0) {
        // It's defined in current scope
        definedVars.add(name);
        definedVars.add(tmps[0]);
      }
      parentVars.add(name);
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
    return this.execute(map, true);
  }

  protected Object execute(Map<String, Object> map, boolean checkExecutionTimeout) {
    if (map == null) {
      map = Collections.emptyMap();
    }
    Env env = genTopEnv(map, checkExecutionTimeout);
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

  protected void setFuncsArgs(final Map<Integer, List<FunctionArgument>> funcsArgs) {
    if (funcsArgs != null) {
      this.funcsArgs = Collections.unmodifiableMap(funcsArgs);
    }
  }

  public Env getCompileEnv() {
    return this.compileEnv;
  }

  protected void setCompileEnv(final Env compileEnv) {
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

  protected void setExpression(final String expression) {
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

  protected Env newEnv(final Map<String, Object> map, final boolean direct,
      boolean checkExecutionTimeout) {
    Env env;
    if (direct) {
      env = new Env(map, map == Collections.EMPTY_MAP ? new HashMap<String, Object>() : map);
    } else {
      env = new Env(map);
    }
    env.configure(this.instance, this, getExecutionStartNs(checkExecutionTimeout), null);
    return env;
  }

  protected Env genTopEnv(final Map<String, Object> map, boolean checkExecutionTimeout) {
    if (map instanceof Env) {
      ((Env) map).configure(this.instance, this, getExecutionStartNs(checkExecutionTimeout), null);
    }
    Env env =
        newEnv(map, this.instance.getOptionValue(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY).bool,
            checkExecutionTimeout);

    if (this.compileEnv != null && !this.compileEnv.isEmpty()) {
      env.putAll(this.compileEnv);
    }
    if (!this.funcsArgs.isEmpty()) {
      env.override(FUNC_PARAMS_VAR, this.funcsArgs);
    }
    return env;
  }

  private long getExecutionStartNs(boolean checkExecutionTimeout) {
    long startNs = -1;
    if (checkExecutionTimeout && this.instance.getOptionValue(Options.EVAL_TIMEOUT_MS).number > 0) {
      startNs = Utils.currentTimeNanos();
    }
    return startNs;
  }

  protected Env newEnv(final Map<String, Object> map) {
    return newEnv(map, false, true);
  }

  public List<String> getFunctionNames() {
    populateFilteredFuncNames();

    return filteredFunctionNames;
  }

  private void populateFilteredFuncNames() {
    if (this.filteredFunctionNames == null) {
      Set<String> validNames = new HashSet<String>(this.functionNames.size());

      for (String funcName : this.functionNames) {
        // Remove internal functions
        if (!funcName.startsWith("__") && !funcName.equals("with_meta")) {
          validNames.add(funcName);
        }
      }

      Set<String> definedFuncs = new HashSet<String>();
      // Find all runtime defined functions
      for (VariableMeta v : this.vars) {
        // TODO: It's not precise, but could work
        if (v.isInit()) {
          definedFuncs.add(v.getName());
        }
      }

      if (this.lambdaBootstraps != null) {
        // Adds sub-expressions function names
        for (LambdaFunctionBootstrap bootstrap : this.lambdaBootstraps.values()) {
          validNames.addAll(bootstrap.getExpression().getFunctionNames());
        }
      }

      // Remove runtime defined functions.
      validNames.removeAll(definedFuncs);

      this.filteredFunctionNames = new ArrayList<>(validNames);
    }
  }

  protected void setFunctionNames(List<String> functionNames) {
    if (functionNames != null) {
      this.functionNames = functionNames;
    }
  }

  public Map<String, LambdaFunctionBootstrap> getLambdaBootstraps() {
    return this.lambdaBootstraps;
  }

  protected void setLambdaBootstraps(final Map<String, LambdaFunctionBootstrap> lambdaBootstraps) {
    this.lambdaBootstraps = lambdaBootstraps;
  }

  public LambdaFunction newLambda(final Env env, final String name) {
    LambdaFunctionBootstrap bootstrap = this.lambdaBootstraps.get(name);
    if (bootstrap == null) {
      throw new ExpressionNotFoundException("Lambda " + name + " not found");
    }
    return bootstrap.newInstance(env);
  }

  @SuppressWarnings("unchecked")
  public void customReadObject(ObjectInputStream input) throws ClassNotFoundException, IOException {
    this.vars = (List<VariableMeta>) input.readObject();
    this.expression = (String) input.readObject();
    this.compileEnv = (Env) input.readObject();
    this.funcsArgs = (Map<Integer, List<FunctionArgument>>) input.readObject();
    this.symbolTable = (SymbolTable) input.readObject();
    this.sourceFile = (String) input.readObject();
    this.lambdaBootstraps = (Map<String, LambdaFunctionBootstrap>) input.readObject();
    this.stringSegs = new ConcurrentHashMap<String, FutureTask<StringSegments>>();
    this.functionNames = (List<String>) input.readObject();
  }

  public void customWriteObject(ObjectOutputStream output) throws IOException {
    output.writeObject(this.vars);
    output.writeObject(this.expression);
    output.writeObject(this.compileEnv);
    output.writeObject(this.funcsArgs);
    output.writeObject(this.symbolTable);
    output.writeObject(this.sourceFile);
    output.writeObject(this.lambdaBootstraps);
    output.writeObject(this.functionNames);
  }

}
