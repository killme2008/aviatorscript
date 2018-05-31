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
import java.math.MathContext;
import java.util.Map;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.runtime.type.AviatorFunction;


/**
 * Avaitor Expression evaluator
 *
 * @author dennis
 *
 */
public final class AviatorEvaluator {

  /**
   * Optimized for compile speed
   */
  public static final int COMPILE = 0;

  /**
   * Optimized for execute speed,this is the default option
   */
  public static final int EVAL = 1;

  /**
   * Aviator version
   */
  public static final String VERSION = "3.0.0";

  /**
   * The global evaluator's functions map.
   *
   * @deprecated please use instance's field.
   * @see AviatorEvaluatorInstance#getFuncMap()
   */
  @Deprecated
  public static final Map<String, Object> FUNC_MAP = getInstance().getFuncMap();

  /**
   * The global evaluator's operators map.
   *
   * @deprecated please use instance's field.
   * @see AviatorEvaluatorInstance#getOpsMap()
   */
  @Deprecated
  public static final Map<OperatorType, AviatorFunction> OPS_MAP = getInstance().getOpsMap();

  /**
   * The global evaluator's byte code version.
   *
   * @deprecated
   * @see AviatorEvaluatorInstance#getBytecodeVersion()
   */
  @Deprecated
  public static int BYTECODE_VER = getInstance().getBytecodeVersion();

  /**
   * Create a aviator evaluator instance.
   *
   * @return
   */
  public static AviatorEvaluatorInstance newInstance() {
    return new AviatorEvaluatorInstance();
  }

  private static class StaticHolder {
    private static AviatorEvaluatorInstance INSTANCE = new AviatorEvaluatorInstance();
  }

  public static AviatorEvaluatorInstance getInstance() {
    return StaticHolder.INSTANCE;
  }

  /**
   * Adds a function loader.
   *
   * @see FunctionLoader
   * @since 4.0.0
   * @param loader
   */
  public static void addFunctionLoader(FunctionLoader loader) {
    getInstance().addFunctionLoader(loader);
  }

  /**
   * Removes a function loader.
   *
   * @see FunctionLoader
   * @since 4.0.0
   * @param loader
   */
  public static void removeFunctionLoader(FunctionLoader loader) {
    getInstance().removeFunctionLoader(loader);
  }

  /**
   * Configure whether to trace code generation
   *
   * @deprecated please use {@link #setOption(Options, Object)}
   * @param t true is to trace,default is false.
   */
  @Deprecated
  public static void setTrace(boolean t) {
    getInstance().setOption(Options.TRACE, t);
  }


  /**
   * Adds a evaluator option
   *
   * @since 2.3.4
   * @see Options
   * @param opt
   * @param val
   */
  public static void setOption(Options opt, Object val) {
    getInstance().setOption(opt, val);
  }


  /**
   * Returns the current evaluator option value, returns null if missing.
   *
   * @param opt
   * @return
   */
  public static <T> T getOption(Options opt) {
    return getInstance().getOption(opt);
  }


  /**
   * Get current trace output stream,default is System.out
   *
   * @return
   */
  public static OutputStream getTraceOutputStream() {
    return getInstance().getTraceOutputStream();
  }


  /**
   * Returns current math context for decimal.
   *
   * @since 2.3.0
   * @deprecated Please use {@link #getOption(Options)}
   * @return
   */
  @Deprecated
  public static MathContext getMathContext() {
    return getInstance().getOption(Options.MATH_CONTEXT);
  }


  /**
   * Set math context for decimal.
   *
   * @param mathContext
   * @deprecated please use {@link #setOption(Options, Object)}
   * @since 2.3.0
   */
  @Deprecated
  public static void setMathContext(MathContext mathContext) {
    getInstance().setOption(Options.MATH_CONTEXT, mathContext);
  }


  /**
   * Set trace output stream
   *
   * @param traceOutputStream
   */
  public static void setTraceOutputStream(OutputStream traceOutputStream) {
    getInstance().setTraceOutputStream(traceOutputStream);
  }



  /**
   * set optimize level,default AviatorEvaluator.EVAL
   *
   * @see #COMPILE
   * @see #EVAL
   * @deprecated please use {@link #setOption(Options, Object)}
   *
   * @param value
   */
  @Deprecated
  public static void setOptimize(int value) {
    getInstance().setOption(Options.OPTIMIZE_LEVEL, value);
  }


  public static void setBYTECODE_VER(int nversion) {
    getInstance().setBytecodeVersion(nversion);
  }


  private AviatorEvaluator() {

  }


  /**
   * Clear all cached compiled expression
   */
  public static void clearExpressionCache() {
    getInstance().clearExpressionCache();
  }


  /**
   * Returns classloader
   *
   * @return
   */
  public static AviatorClassLoader getAviatorClassLoader() {
    return getInstance().getAviatorClassLoader();
  }


  /**
   * Returns classloader
   *
   * @return
   */
  public static AviatorClassLoader getAviatorClassLoader(boolean cached) {
    return getInstance().getAviatorClassLoader(cached);
  }


  /**
   * Add an aviator function,it's not thread-safe.
   *
   * @param function
   */
  public static void addFunction(AviatorFunction function) {
    getInstance().addFunction(function);
  }

  /**
   * Define a function by name and expression.
   *
   * @param name the function name
   * @param expression the expression to be executed and it's result must be a function.
   * @since 4.0.0
   */
  public void defineFunction(String name, String expression) {
    getInstance().defineFunction(name, expression);
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
    getInstance().defineFunction(name, expression, env);
  }


  /**
   * Remove an aviator function by name,it's not thread-safe.
   *
   * @param name
   * @return
   */
  public static AviatorFunction removeFunction(String name) {
    return getInstance().removeFunction(name);
  }


  /**
   * Retrieve an aviator function by name,throw exception if not found or null.It's not thread-safe.
   *
   * @param name
   * @return
   */
  public static AviatorFunction getFunction(String name) {
    return getInstance().getFunction(name);
  }

  /**
   * Add an operator aviator function,it's not thread-safe.
   *
   * @param function
   */
  public static void addOpFunction(OperatorType opType, AviatorFunction function) {
    getInstance().addOpFunction(opType, function);
  }


  /**
   * Retrieve an operator aviator function by op type, return null if not found.It's not
   * thread-safe.
   *
   * @since 3.3
   * @param opType
   * @return
   */
  public static AviatorFunction getOpFunction(OperatorType opType) {
    return getInstance().getOpFunction(opType);
  }

  /**
   * Remove an operator aviator function by op type, it's not thread-safe.
   *
   * @since 3.3
   * @param opType
   * @return
   */
  public static AviatorFunction removeOpFunction(OperatorType opType) {
    return getInstance().removeOpFunction(opType);
  }


  /**
   * Check if the function is existed in aviator
   *
   * @param name
   * @return
   */
  public static boolean containsFunction(String name) {
    return getInstance().containsFunction(name);
  }

  /**
   * Remove a aviator function
   *
   * @param function
   * @return
   */
  public static AviatorFunction removeFunction(AviatorFunction function) {
    return getInstance().removeFunction(function);
  }


  /**
   * Configure user defined classloader
   *
   * @deprecated
   * @param aviatorClassLoader
   */
  @Deprecated
  public static void setAviatorClassLoader(AviatorClassLoader aviatorClassLoader) {
    // AviatorEvaluator.aviatorClassLoader = aviatorClassLoader;
  }


  /**
   * Returns a compiled expression in cache
   *
   * @param expression
   * @return
   */
  public static Expression getCachedExpression(String expression) {
    return getInstance().getCachedExpression(expression);
  }


  /**
   * Compile a text expression to Expression object
   *
   * @param expression text expression
   * @param cached Whether to cache the compiled result,make true to cache it.
   * @return
   */
  public static Expression compile(final String expression, final boolean cached) {
    return getInstance().compile(expression, cached);
  }


  /**
   * Compile a text expression to Expression Object without caching
   *
   * @param expression
   * @return
   */
  public static Expression compile(String expression) {
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
  public static Object exec(String expression, Object... values) {
    return getInstance().exec(expression, values);
  }


  /**
   * Execute a text expression with environment
   *
   * @param expression text expression
   * @param env Binding variable environment
   * @param cached Whether to cache the compiled result,make true to cache it.
   */
  public static Object execute(String expression, Map<String, Object> env, boolean cached) {
    return getInstance().execute(expression, env, cached);
  }


  /**
   * Execute a text expression without caching
   *
   * @param expression
   * @param env
   * @return
   */
  public static Object execute(String expression, Map<String, Object> env) {
    return execute(expression, env, false);
  }


  /**
   * Invalidate expression cache
   *
   * @param expression
   */
  public static void invalidateCache(String expression) {
    getInstance().invalidateCache(expression);
  }


  /**
   * Execute a text expression without caching and env map.
   *
   * @param expression
   * @return
   */
  public static Object execute(String expression) {
    return execute(expression, (Map<String, Object>) null);
  }
}
