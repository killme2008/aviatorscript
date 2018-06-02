
/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator;


import java.io.OutputStream;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.IdentityHashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.Callable;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.FutureTask;
import com.googlecode.aviator.asm.Opcodes;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.code.OptimizeCodeGenerator;
import com.googlecode.aviator.code.asm.ASMCodeGenerator;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.ExpressionLexer;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.ExpressionParser;
import com.googlecode.aviator.runtime.function.math.MathAbsFunction;
import com.googlecode.aviator.runtime.function.math.MathCosFunction;
import com.googlecode.aviator.runtime.function.math.MathLog10Function;
import com.googlecode.aviator.runtime.function.math.MathLogFunction;
import com.googlecode.aviator.runtime.function.math.MathPowFunction;
import com.googlecode.aviator.runtime.function.math.MathRoundFunction;
import com.googlecode.aviator.runtime.function.math.MathSinFunction;
import com.googlecode.aviator.runtime.function.math.MathSqrtFunction;
import com.googlecode.aviator.runtime.function.math.MathTanFunction;
import com.googlecode.aviator.runtime.function.seq.SeqCompsitePredFunFunction;
import com.googlecode.aviator.runtime.function.seq.SeqCompsitePredFunFunction.LogicOp;
import com.googlecode.aviator.runtime.function.seq.SeqCountFunction;
import com.googlecode.aviator.runtime.function.seq.SeqEveryFunction;
import com.googlecode.aviator.runtime.function.seq.SeqFilterFunction;
import com.googlecode.aviator.runtime.function.seq.SeqIncludeFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMakePredicateFunFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMapFunction;
import com.googlecode.aviator.runtime.function.seq.SeqNotAnyFunction;
import com.googlecode.aviator.runtime.function.seq.SeqReduceFunction;
import com.googlecode.aviator.runtime.function.seq.SeqSomeFunction;
import com.googlecode.aviator.runtime.function.seq.SeqSortFunction;
import com.googlecode.aviator.runtime.function.string.StringContainsFunction;
import com.googlecode.aviator.runtime.function.string.StringEndsWithFunction;
import com.googlecode.aviator.runtime.function.string.StringIndexOfFunction;
import com.googlecode.aviator.runtime.function.string.StringJoinFunction;
import com.googlecode.aviator.runtime.function.string.StringLengthFunction;
import com.googlecode.aviator.runtime.function.string.StringReplaceAllFunction;
import com.googlecode.aviator.runtime.function.string.StringReplaceFirstFunction;
import com.googlecode.aviator.runtime.function.string.StringSplitFunction;
import com.googlecode.aviator.runtime.function.string.StringStartsWithFunction;
import com.googlecode.aviator.runtime.function.string.StringSubStringFunction;
import com.googlecode.aviator.runtime.function.system.BinaryFunction;
import com.googlecode.aviator.runtime.function.system.BooleanFunction;
import com.googlecode.aviator.runtime.function.system.Date2StringFunction;
import com.googlecode.aviator.runtime.function.system.DoubleFunction;
import com.googlecode.aviator.runtime.function.system.LongFunction;
import com.googlecode.aviator.runtime.function.system.NowFunction;
import com.googlecode.aviator.runtime.function.system.PrintFunction;
import com.googlecode.aviator.runtime.function.system.PrintlnFunction;
import com.googlecode.aviator.runtime.function.system.RandomFunction;
import com.googlecode.aviator.runtime.function.system.StrFunction;
import com.googlecode.aviator.runtime.function.system.String2DateFunction;
import com.googlecode.aviator.runtime.function.system.SysDateFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;


/**
 * A aviator evaluator instance
 *
 * @since 4.0.0
 * @author dennis
 *
 */
public final class AviatorEvaluatorInstance {

  private AviatorClassLoader aviatorClassLoader;

  private OutputStream traceOutputStream = System.out;

  /**
   * Generated java class version,default 1.6
   */
  public int bytecodeVersion = Opcodes.V1_6;

  private final ConcurrentHashMap<Options, Object> options =
      new ConcurrentHashMap<Options, Object>();
  /** function loader list */
  private List<FunctionLoader> functionLoaders;


  /**
   * Adds a function loader
   *
   * @see FunctionLoader
   * @since 4.0.0
   * @param loader
   */
  public void addFunctionLoader(FunctionLoader loader) {
    if (this.functionLoaders == null) {
      functionLoaders = new ArrayList<FunctionLoader>();
    }
    functionLoaders.add(loader);
  }

  /**
   * Remove a function loader
   *
   * @since 4.0.0
   * @param loader
   */
  public void removeFunctionLoader(FunctionLoader loader) {
    if (this.functionLoaders == null) {
      return;
    }
    this.functionLoaders.remove(loader);
  }

  /**
   * Adds a evaluator option
   *
   * @since 2.3.4
   * @see Options
   * @param opt
   * @param val
   */
  public void setOption(Options opt, Object val) {
    if (opt == null || val == null) {
      throw new IllegalArgumentException("Option and value should not be null.");
    }
    if (!opt.isValidValue(val)) {
      throw new IllegalArgumentException("Invalid value for option:" + opt.name());
    }
    options.put(opt, val);
  }


  /**
   * Returns the current evaluator option value, returns null if missing.
   *
   * @param opt
   * @return
   */
  @SuppressWarnings("unchecked")
  public <T> T getOption(Options opt) {
    Object val = options.get(opt);
    if (val == null) {
      val = opt.getDefaultValue();
    }
    return (T) val;
  }


  /**
   * Returns the generated java classes byte code version, 1.6 by defualt.
   *
   * @return
   */
  public int getBytecodeVersion() {
    return bytecodeVersion;
  }


  /**
   * Set the generated java classes java byte code version.
   *
   * @see Opcodes#V1_6
   * @param bytecodeVersion
   */
  public void setBytecodeVersion(int bytecodeVersion) {
    this.bytecodeVersion = bytecodeVersion;
  }


  /**
   * Get the evaluator instance options
   *
   * @return
   */
  public ConcurrentHashMap<Options, Object> getOptions() {
    return options;
  }


  /**
   * Returns the functions map
   *
   * @return
   */
  public Map<String, Object> getFuncMap() {
    return funcMap;
  }


  /**
   * Returns the operators map.
   *
   * @return
   */
  public Map<OperatorType, AviatorFunction> getOpsMap() {
    return opsMap;
  }


  /**
   * Get current trace output stream,default is System.out
   *
   * @return
   */
  public OutputStream getTraceOutputStream() {
    return traceOutputStream;
  }


  /**
   * Set trace output stream
   *
   * @param traceOutputStream
   */
  public void setTraceOutputStream(OutputStream traceOutputStream) {
    this.traceOutputStream = traceOutputStream;
  }

  {
    aviatorClassLoader = AccessController.doPrivileged(new PrivilegedAction<AviatorClassLoader>() {

      @Override
      public AviatorClassLoader run() {
        return new AviatorClassLoader(AviatorEvaluatorInstance.class.getClassLoader());
      }

    });
  }

  private final Map<String, Object> funcMap = new HashMap<String, Object>();

  private final Map<OperatorType, AviatorFunction> opsMap =
      new IdentityHashMap<OperatorType, AviatorFunction>();


  private void loadLib() {
    // Load internal functions
    // load sys lib
    addFunction(new SysDateFunction());
    addFunction(new PrintlnFunction());
    addFunction(new PrintFunction());
    addFunction(new RandomFunction());
    addFunction(new NowFunction());
    addFunction(new LongFunction());
    addFunction(new BooleanFunction());
    addFunction(new DoubleFunction());
    addFunction(new StrFunction());
    addFunction(new Date2StringFunction());
    addFunction(new String2DateFunction());
    addFunction(new BinaryFunction(OperatorType.ADD));
    addFunction(new BinaryFunction(OperatorType.SUB));
    addFunction(new BinaryFunction(OperatorType.MULT));
    addFunction(new BinaryFunction(OperatorType.DIV));
    addFunction(new BinaryFunction(OperatorType.MOD));
    addFunction(new BinaryFunction(OperatorType.NEG));
    addFunction(new BinaryFunction(OperatorType.NOT));
    addFunction(new BinaryFunction(OperatorType.BIT_AND));
    addFunction(new BinaryFunction(OperatorType.BIT_OR));
    addFunction(new BinaryFunction(OperatorType.BIT_XOR));
    addFunction(new BinaryFunction(OperatorType.BIT_NOT));

    // load string lib
    addFunction(new StringContainsFunction());
    addFunction(new StringIndexOfFunction());
    addFunction(new StringStartsWithFunction());
    addFunction(new StringEndsWithFunction());
    addFunction(new StringSubStringFunction());
    addFunction(new StringLengthFunction());
    addFunction(new StringSplitFunction());
    addFunction(new StringJoinFunction());
    addFunction(new StringReplaceFirstFunction());
    addFunction(new StringReplaceAllFunction());

    // load math lib
    addFunction(new MathAbsFunction());
    addFunction(new MathRoundFunction());
    addFunction(new MathPowFunction());
    addFunction(new MathSqrtFunction());
    addFunction(new MathLog10Function());
    addFunction(new MathLogFunction());
    addFunction(new MathSinFunction());
    addFunction(new MathCosFunction());
    addFunction(new MathTanFunction());

    // seq lib
    addFunction(new SeqMapFunction());
    addFunction(new SeqReduceFunction());
    addFunction(new SeqFilterFunction());
    addFunction(new SeqSortFunction());
    addFunction(new SeqIncludeFunction());
    addFunction(new SeqCountFunction());
    addFunction(new SeqEveryFunction());
    addFunction(new SeqNotAnyFunction());
    addFunction(new SeqSomeFunction());
    addFunction(new SeqMakePredicateFunFunction("seq.eq", OperatorType.EQ));
    addFunction(new SeqMakePredicateFunFunction("seq.neq", OperatorType.NEQ));
    addFunction(new SeqMakePredicateFunFunction("seq.lt", OperatorType.LT));
    addFunction(new SeqMakePredicateFunFunction("seq.le", OperatorType.LE));
    addFunction(new SeqMakePredicateFunFunction("seq.gt", OperatorType.GT));
    addFunction(new SeqMakePredicateFunFunction("seq.ge", OperatorType.GE));
    addFunction(new SeqCompsitePredFunFunction("seq.and", LogicOp.AND));
    addFunction(new SeqCompsitePredFunFunction("seq.or", LogicOp.OR));
    addFunction(new SeqMakePredicateFunFunction("seq.true", OperatorType.EQ, AviatorBoolean.TRUE));
    addFunction(
        new SeqMakePredicateFunFunction("seq.false", OperatorType.EQ, AviatorBoolean.FALSE));
    addFunction(new SeqMakePredicateFunFunction("seq.nil", OperatorType.EQ, AviatorNil.NIL));
    addFunction(new SeqMakePredicateFunFunction("seq.exists", OperatorType.NEQ, AviatorNil.NIL));
  }

  /**
   * Compiled Expression cache
   */
  private final ConcurrentHashMap<String/* text expression */, FutureTask<Expression>/*
                                                                                     * Compiled
                                                                                     * expression
                                                                                     * task
                                                                                     */> cacheExpressions =
      new ConcurrentHashMap<String, FutureTask<Expression>>();



  /**
   * Create a aviator evaluator instance.
   */
  AviatorEvaluatorInstance() {
    this.loadLib();
    this.addFunctionLoader(ClassPathConfigFunctionLoader.getInstance());
  }

  /**
   * Clear all cached compiled expression
   */
  public void clearExpressionCache() {
    cacheExpressions.clear();
  }


  /**
   * Returns classloader
   *
   * @return
   */
  public AviatorClassLoader getAviatorClassLoader() {
    return getAviatorClassLoader(false);
  }


  /**
   * Returns classloader
   *
   * @return
   */
  public AviatorClassLoader getAviatorClassLoader(boolean cached) {
    if (cached) {
      return aviatorClassLoader;
    } else {
      return new AviatorClassLoader(Thread.currentThread().getContextClassLoader());
    }
  }


  /**
   * Add an aviator function,it's not thread-safe.
   *
   * @param function
   */
  public void addFunction(AviatorFunction function) {
    final String name = function.getName();
    addFunction(name, function);
  }

  /**
   * Adds a function with the name
   *
   * @param name
   * @param function
   */
  public void addFunction(final String name, AviatorFunction function) {
    if ("lambda".equals(name)) {
      throw new IllegalArgumentException("Invalid function name, lambda is a keyword.");
    }
    if (funcMap.containsKey(name)) {
      System.out.println("[Aviator WARN] The function '" + name
          + "' is already exists, but is replaced with new one.");
    }
    funcMap.put(name, function);
  }

  /**
   * Define a function by name and expression.
   *
   * @param name the function name
   * @param expression the expression to be executed and it's result must be a function.
   * @since 4.0.0
   */
  public void defineFunction(String name, String expression) {
    this.defineFunction(name, expression, null);
  }

  /**
   * Define a function by name and expression with the execution env.
   *
   * @param name the function name
   * @param expression the expression to be executed and it's result must be a function.
   * @param env the expression execution env
   * @since 4.0.0
   */
  public void defineFunction(String name, String expression, Map<String, Object> env) {
    AviatorFunction function = (AviatorFunction) this.execute(expression, env);
    this.addFunction(name, function);
  }


  /**
   * Remove an aviator function by name,it's not thread-safe.
   *
   * @param name
   * @return
   */
  public AviatorFunction removeFunction(String name) {
    return (AviatorFunction) funcMap.remove(name);
  }


  /**
   * Retrieve an aviator function by name,throw exception if not found or null.It's not thread-safe.
   *
   * @param name
   * @return
   */
  public AviatorFunction getFunction(String name) {
    AviatorFunction function = (AviatorFunction) funcMap.get(name);
    if (function == null && functionLoaders != null) {
      for (FunctionLoader loader : functionLoaders) {
        function = loader.onFunctionNotFound(name);
        if (function != null) {
          break;
        }
      }
    }
    if (function == null) {
      throw new ExpressionRuntimeException("Could not find function named '" + name + "'");
    }
    return function;
  }

  /**
   * Add an operator aviator function,it's not thread-safe.
   *
   * @param function
   */
  public void addOpFunction(OperatorType opType, AviatorFunction function) {
    opsMap.put(opType, function);
  }


  /**
   * Retrieve an operator aviator function by op type, return null if not found.It's not
   * thread-safe.
   *
   * @since 3.3
   * @param opType
   * @return
   */
  public AviatorFunction getOpFunction(OperatorType opType) {
    return opsMap.get(opType);
  }

  /**
   * Remove an operator aviator function by op type, it's not thread-safe.
   *
   * @since 3.3
   * @param opType
   * @return
   */
  public AviatorFunction removeOpFunction(OperatorType opType) {
    return opsMap.remove(opType);
  }


  /**
   * Check if the function is existed in aviator
   *
   * @param name
   * @return
   */
  public boolean containsFunction(String name) {
    return funcMap.containsKey(name);
  }

  /**
   * Remove a aviator function
   *
   * @param function
   * @return
   */
  public AviatorFunction removeFunction(AviatorFunction function) {
    return removeFunction(function.getName());
  }


  /**
   * Returns a compiled expression in cache
   *
   * @param expression
   * @return
   */
  public Expression getCachedExpression(String expression) {
    FutureTask<Expression> task = cacheExpressions.get(expression);
    if (task != null) {
      return getCompiledExpression(expression, task);
    } else {
      return null;
    }
  }


  /**
   * Compile a text expression to Expression object
   *
   * @param expression text expression
   * @param cached Whether to cache the compiled result,make true to cache it.
   * @return
   */
  public Expression compile(final String expression, final boolean cached) {
    if (expression == null || expression.trim().length() == 0) {
      throw new CompileExpressionErrorException("Blank expression");
    }

    if (cached) {
      FutureTask<Expression> task = cacheExpressions.get(expression);
      if (task != null) {
        return getCompiledExpression(expression, task);
      }
      task = new FutureTask<Expression>(new Callable<Expression>() {
        @Override
        public Expression call() throws Exception {
          return innerCompile(expression, cached);
        }

      });
      FutureTask<Expression> existedTask = cacheExpressions.putIfAbsent(expression, task);
      if (existedTask == null) {
        existedTask = task;
        existedTask.run();
      }
      return getCompiledExpression(expression, existedTask);

    } else {
      return innerCompile(expression, cached);
    }

  }


  private Expression getCompiledExpression(final String expression, FutureTask<Expression> task) {
    try {
      return task.get();
    } catch (Exception e) {
      cacheExpressions.remove(expression);
      throw new CompileExpressionErrorException("Compile expression failure:" + expression, e);
    }
  }


  private Expression innerCompile(final String expression, boolean cached) {
    ExpressionLexer lexer = new ExpressionLexer(this, expression);
    CodeGenerator codeGenerator = newCodeGenerator(cached);
    ExpressionParser parser = new ExpressionParser(this, lexer, codeGenerator);
    Expression exp = parser.parse();
    if ((boolean) getOption(Options.TRACE_EVAL)) {
      ((BaseExpression) exp).setExpression(expression);
    }
    return exp;
  }

  private int getOptimizeLevel() {
    return getOption(Options.OPTIMIZE_LEVEL);
  }


  public CodeGenerator newCodeGenerator(boolean cached) {
    AviatorClassLoader classLoader = getAviatorClassLoader(cached);
    return newCodeGenerator(classLoader);

  }

  public CodeGenerator newCodeGenerator(AviatorClassLoader classLoader) {
    switch (getOptimizeLevel()) {
      case AviatorEvaluator.COMPILE:
        ASMCodeGenerator asmCodeGenerator = new ASMCodeGenerator(this, classLoader,
            traceOutputStream, (Boolean) getOption(Options.TRACE));
        asmCodeGenerator.start();
        return asmCodeGenerator;
      case AviatorEvaluator.EVAL:
        return new OptimizeCodeGenerator(this, classLoader, traceOutputStream,
            (Boolean) getOption(Options.TRACE));
      default:
        throw new IllegalArgumentException("Unknow option " + getOptimizeLevel());
    }
  }


  /**
   * Compile a text expression to Expression Object without caching
   *
   * @param expression
   * @return
   */
  public Expression compile(String expression) {
    return compile(expression, false);
  }


  /**
   * Execute a text expression with values that are variables order in the expression.It only runs
   * in EVAL mode,and it will cache the compiled expression.
   *
   * @param expression
   * @param values
   * @return
   */
  public Object exec(String expression, Object... values) {
    if (getOptimizeLevel() != AviatorEvaluator.EVAL) {
      throw new IllegalStateException("Aviator evaluator is not in EVAL mode.");
    }
    Expression compiledExpression = compile(expression, true);
    if (compiledExpression != null) {
      List<String> vars = compiledExpression.getVariableNames();
      if (!vars.isEmpty()) {
        int valLen = values == null ? 0 : values.length;
        if (valLen != vars.size()) {
          throw new IllegalArgumentException("Expect " + vars.size() + " values,but has " + valLen);
        }
        Map<String, Object> env = new HashMap<String, Object>();
        int i = 0;
        for (String var : vars) {
          env.put(var, values[i++]);
        }
        return compiledExpression.execute(env);
      } else {
        return compiledExpression.execute();
      }
    } else {
      throw new ExpressionRuntimeException("Null compiled expression for " + expression);
    }
  }


  /**
   * Execute a text expression with environment
   *
   * @param expression text expression
   * @param env Binding variable environment
   * @param cached Whether to cache the compiled result,make true to cache it.
   */
  public Object execute(String expression, Map<String, Object> env, boolean cached) {
    Expression compiledExpression = compile(expression, cached);
    if (compiledExpression != null) {
      return compiledExpression.execute(env);
    } else {
      throw new ExpressionRuntimeException("Null compiled expression for " + expression);
    }
  }


  /**
   * Execute a text expression without caching
   *
   * @param expression
   * @param env
   * @return
   */
  public Object execute(String expression, Map<String, Object> env) {
    return execute(expression, env, false);
  }


  /**
   * Invalidate expression cache
   *
   * @param expression
   */
  public void invalidateCache(String expression) {
    cacheExpressions.remove(expression);
  }


  /**
   * Execute a text expression without caching and env map.
   *
   * @param expression
   * @return
   */
  public Object execute(String expression) {
    return execute(expression, (Map<String, Object>) null);
  }
}
