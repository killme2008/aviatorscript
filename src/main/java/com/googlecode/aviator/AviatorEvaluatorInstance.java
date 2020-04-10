
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


import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.Reader;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.net.URL;
import java.nio.charset.Charset;
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
import com.googlecode.aviator.Options.Value;
import com.googlecode.aviator.annotation.Function;
import com.googlecode.aviator.annotation.Ignore;
import com.googlecode.aviator.annotation.Import;
import com.googlecode.aviator.annotation.ImportScope;
import com.googlecode.aviator.asm.Opcodes;
import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.code.OptimizeCodeGenerator;
import com.googlecode.aviator.code.asm.ASMCodeGenerator;
import com.googlecode.aviator.exception.CompileExpressionErrorException;
import com.googlecode.aviator.exception.ExpressionNotFoundException;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import com.googlecode.aviator.lexer.ExpressionLexer;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.parser.AviatorClassLoader;
import com.googlecode.aviator.parser.ExpressionParser;
import com.googlecode.aviator.runtime.function.ClassMethodFunction;
import com.googlecode.aviator.runtime.function.internal.IfCallccFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerBreakFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerContFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerFunction;
import com.googlecode.aviator.runtime.function.internal.ReducerReturnFunction;
import com.googlecode.aviator.runtime.function.math.MathAbsFunction;
import com.googlecode.aviator.runtime.function.math.MathCosFunction;
import com.googlecode.aviator.runtime.function.math.MathLog10Function;
import com.googlecode.aviator.runtime.function.math.MathLogFunction;
import com.googlecode.aviator.runtime.function.math.MathPowFunction;
import com.googlecode.aviator.runtime.function.math.MathRoundFunction;
import com.googlecode.aviator.runtime.function.math.MathSinFunction;
import com.googlecode.aviator.runtime.function.math.MathSqrtFunction;
import com.googlecode.aviator.runtime.function.math.MathTanFunction;
import com.googlecode.aviator.runtime.function.seq.SeqAddFunction;
import com.googlecode.aviator.runtime.function.seq.SeqArrayFunction;
import com.googlecode.aviator.runtime.function.seq.SeqCompsitePredFunFunction;
import com.googlecode.aviator.runtime.function.seq.SeqCompsitePredFunFunction.LogicOp;
import com.googlecode.aviator.runtime.function.seq.SeqContainsKeyFunction;
import com.googlecode.aviator.runtime.function.seq.SeqCountFunction;
import com.googlecode.aviator.runtime.function.seq.SeqEveryFunction;
import com.googlecode.aviator.runtime.function.seq.SeqFilterFunction;
import com.googlecode.aviator.runtime.function.seq.SeqGetFunction;
import com.googlecode.aviator.runtime.function.seq.SeqIncludeFunction;
import com.googlecode.aviator.runtime.function.seq.SeqIntoFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMakePredicateFunFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMapEntryFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMapFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMaxFunction;
import com.googlecode.aviator.runtime.function.seq.SeqMinFunction;
import com.googlecode.aviator.runtime.function.seq.SeqNewArrayFunction;
import com.googlecode.aviator.runtime.function.seq.SeqNewListFunction;
import com.googlecode.aviator.runtime.function.seq.SeqNewMapFunction;
import com.googlecode.aviator.runtime.function.seq.SeqNewSetFunction;
import com.googlecode.aviator.runtime.function.seq.SeqNotAnyFunction;
import com.googlecode.aviator.runtime.function.seq.SeqPutFunction;
import com.googlecode.aviator.runtime.function.seq.SeqReduceFunction;
import com.googlecode.aviator.runtime.function.seq.SeqRemoveFunction;
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
import com.googlecode.aviator.runtime.function.system.AssertFunction;
import com.googlecode.aviator.runtime.function.system.BinaryFunction;
import com.googlecode.aviator.runtime.function.system.BooleanFunction;
import com.googlecode.aviator.runtime.function.system.CompareFunction;
import com.googlecode.aviator.runtime.function.system.Date2StringFunction;
import com.googlecode.aviator.runtime.function.system.DoubleFunction;
import com.googlecode.aviator.runtime.function.system.IdentityFunction;
import com.googlecode.aviator.runtime.function.system.IsDefFunction;
import com.googlecode.aviator.runtime.function.system.LoadFunction;
import com.googlecode.aviator.runtime.function.system.LongFunction;
import com.googlecode.aviator.runtime.function.system.MaxFunction;
import com.googlecode.aviator.runtime.function.system.MinFunction;
import com.googlecode.aviator.runtime.function.system.NowFunction;
import com.googlecode.aviator.runtime.function.system.PrintFunction;
import com.googlecode.aviator.runtime.function.system.PrintlnFunction;
import com.googlecode.aviator.runtime.function.system.RandomFunction;
import com.googlecode.aviator.runtime.function.system.RangeFunction;
import com.googlecode.aviator.runtime.function.system.RequireFunction;
import com.googlecode.aviator.runtime.function.system.StrFunction;
import com.googlecode.aviator.runtime.function.system.String2DateFunction;
import com.googlecode.aviator.runtime.function.system.SysDateFunction;
import com.googlecode.aviator.runtime.function.system.TupleFunction;
import com.googlecode.aviator.runtime.function.system.TypeFunction;
import com.googlecode.aviator.runtime.function.system.UndefFunction;
import com.googlecode.aviator.runtime.module.IoModule;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.LRUMap;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.Utils;


/**
 * A aviator evaluator instance
 *
 * @since 4.0.0
 * @author dennis
 *
 */
public final class AviatorEvaluatorInstance {

  private volatile AviatorClassLoader aviatorClassLoader = initAviatorClassLoader();

  private OutputStream traceOutputStream = System.out;

  private FunctionMissing functionMissing;

  /**
   * Generated java class version,default 1.7
   */
  private int bytecodeVersion = Opcodes.V1_7;

  /**
   * Options
   */
  private volatile Map<Options, Value> options = new IdentityHashMap<Options, Value>();


  /** function loader list */
  private List<FunctionLoader> functionLoaders;


  /**
   * Adds a function loader
   *
   * @see FunctionLoader
   * @since 4.0.0
   * @param loader
   */
  public void addFunctionLoader(final FunctionLoader loader) {
    if (this.functionLoaders == null) {
      this.functionLoaders = new ArrayList<FunctionLoader>();
    }
    this.functionLoaders.add(loader);
  }

  /**
   * Compile a script file into expression.
   *
   * @param file the script file path
   * @param cached whether to cached the compiled result with key is script file's absolute path.
   * @since 5.0.0
   * @return the compiled expression instance.
   */
  public Expression compileScript(final String path, final boolean cached) throws IOException {
    File file = tryFindScriptFile(path);
    return compileScript(file.getAbsolutePath(), file, cached);
  }

  /**
   * Compile a script into expression.
   *
   * @param cacheKey caching key when cached is true.
   * @param file the script file
   * @param cached whether to cache the expression instance by cacheKey.
   * @return the compiled expression instance.
   * @since 5.0.0
   * @throws IOException
   */
  public Expression compileScript(final String cacheKey, final File file, final boolean cached)
      throws IOException {
    try (InputStream in = new FileInputStream(file);
        Reader reader = new InputStreamReader(in, Charset.forName("utf-8"));) {

      return compile(cacheKey, Utils.readFully(reader), cached);
    }
  }

  private File tryFindScriptFile(final String path) throws IOException {
    // 1. absolute path
    File file = new File(path);
    if (file.exists()) {
      return file;
    }
    // 2. from context classloader
    ClassLoader contextLoader = Thread.currentThread().getContextClassLoader();
    file = tryFindFileFromClassLoader(path, contextLoader);
    if (file != null) {
      return file;
    }
    // 3. from current class loader
    contextLoader = getClass().getClassLoader();
    file = tryFindFileFromClassLoader(path, contextLoader);
    if (file != null) {
      return file;
    }
    throw new FileNotFoundException("File not found: " + path);
  }

  /**
   * Set a custom aviator class loader
   *
   * @since 5.0.0
   * @param aviatorClassLoader
   */
  public void setAviatorClassLoader(final AviatorClassLoader aviatorClassLoader) {
    this.aviatorClassLoader = aviatorClassLoader;
  }

  private File tryFindFileFromClassLoader(final String path, final ClassLoader contextLoader) {
    URL url = contextLoader.getResource(path);
    if (url != null) {
      return new File(url.getPath());
    }
    if (!path.startsWith("/")) {
      url = contextLoader.getResource("/" + path);
    }
    return null;
  }

  /**
   * Loads a script from path and return it's exports.
   *
   * @param path
   * @param args arguments to execute the script.
   * @throws IOException
   * @return the exports map.
   * @since 5.0.0
   */
  public Map<String, Object> loadScript(final String path) throws IOException {
    final File file = tryFindScriptFile(path);
    final String abPath = file.getAbsolutePath();
    return loadScript0(abPath);
  }

  @SuppressWarnings("unchecked")
  private Map<String, Object> loadScript0(final String abPath) throws IOException {
    Expression exp = this.compileScript(abPath);
    final Env exports = new Env();
    final Map<String, Object> module = exp.newEnv("exports", exports, "path", abPath);
    Map<String, Object> env = exp.newEnv("__MODULE__", module, "exports", exports);
    exp.execute(env);
    exports.setInstance(this);
    return (Map<String, Object>) module.get("exports");
  }


  /**
   * Loads a script from path and return it's exports with module caching.
   *
   * @param path
   * @param args arguments to execute the script.
   * @throws IOException
   * @return the exports map
   * @since 5.0.0
   */
  public Map<String, Object> requireScript(final String path) throws IOException {
    File file = tryFindScriptFile(path);
    final String abPath = file.getAbsolutePath();
    Map<String, Object> exports = (Env) this.moduleCache.get(abPath);
    if (exports != null) {
      return exports;
    } else {
      // TODO better lock
      synchronized (abPath.intern()) {
        exports = (Env) this.moduleCache.get(abPath);
        if (exports != null) {
          return exports;
        }
        exports = loadScript0(abPath);
        this.moduleCache.put(abPath, exports);
        return exports;
      }
    }
  }

  /**
   * Adds a module class and import it's public static methods as module's exports into module
   * cache, return the exports map.
   *
   * @param moduleClazz
   * @return the exports map
   * @since 5.0.0
   */
  public Env addModule(final Class<?> moduleClazz)
      throws NoSuchMethodException, IllegalAccessException {
    String namespace = moduleClazz.getSimpleName();

    Import importAnt = moduleClazz.getAnnotation(Import.class);

    if (importAnt != null) {
      namespace = importAnt.ns();
      if (namespace == null || namespace.isEmpty()
          || !ExpressionParser.isJavaIdentifier(namespace)) {
        throw new IllegalArgumentException("Invalid namespace in Import annotation: " + namespace);
      }
    }

    Env exports = null;
    synchronized (namespace.intern()) {
      exports = (Env) this.moduleCache.get(namespace);
      if (exports != null) {
        return exports;
      }
      exports = loadModule(moduleClazz);
      this.moduleCache.put(namespace, exports);
      return exports;
    }
  }

  private Env loadModule(final Class<?> moduleClazz)
      throws IllegalAccessException, NoSuchMethodException {
    Map<String, List<Method>> methodMap = findMethodsFromClass(true, moduleClazz);

    if (methodMap == null || methodMap.isEmpty()) {
      throw new IllegalArgumentException("Empty module");
    }

    Env exports = new Env();

    for (Map.Entry<String, List<Method>> entry : methodMap.entrySet()) {
      exports.put(entry.getKey(), new ClassMethodFunction(moduleClazz, true, entry.getKey(),
          entry.getKey(), entry.getValue()));
    }
    exports.setInstance(this);
    return exports;
  }

  public Map<String, Object> getModuleCache() {
    return this.moduleCache;
  }

  /**
   * Compile a script file into expression, it doesn't cache the compiled result.
   *
   * @param file the script file path
   * @return
   */
  public Expression compileScript(final String path) throws IOException {
    return this.compileScript(path, false);
  }


  /**
   * Returns the function missing handler, null if not set.
   *
   * @since 4.2.5
   * @return
   */
  public FunctionMissing getFunctionMissing() {
    return this.functionMissing;
  }


  /**
   * Configure a function missing handler.
   *
   * @since 4.2.5
   * @param functionMissing
   */
  public void setFunctionMissing(final FunctionMissing functionMissing) {
    if (this.functionMissing != null) {
      throw new IllegalArgumentException("functionMissing already set:" + this.functionMissing);
    }
    this.functionMissing = functionMissing;
  }



  /**
   * Adds all public instance methods in the class as custom functions into evaluator except those
   * have {@link Ignore} annotation, all these functions will keep the same name as method name, but
   * prefixed with namespace, the function name can be renamed by {@link Function} annotation. And
   * the function will have more than one argument than the method, the function's first argument is
   * always the instance object(this pointer).
   *
   * @since 4.2.3
   * @see Ignore
   * @see Function
   * @param namespace the functions namespace
   * @param clazz the class
   * @return the added function list.
   */
  public List<String> addInstanceFunctions(final String namespace, final Class<?> clazz)
      throws IllegalAccessException, NoSuchMethodException {
    return addMethodFunctions(namespace, false, clazz);
  }

  private List<String> addMethodFunctions(final String namespace, final boolean isStatic,
      final Class<?> clazz) throws IllegalAccessException, NoSuchMethodException {
    Map<String, List<Method>> methodMap = findMethodsFromClass(isStatic, clazz);
    List<String> added = new ArrayList<>();

    for (Map.Entry<String, List<Method>> entry : methodMap.entrySet()) {
      String methodName = entry.getKey();
      String name = namespace + "." + methodName;
      this.addFunction(
          new ClassMethodFunction(clazz, isStatic, name, methodName, entry.getValue()));
      added.add(name);
    }
    return added;
  }

  /**
   * Adds all public static methods in the class as custom functions into evaluator except those
   * have {@link Ignore} annotation, all these functions will keep the same name as method name, but
   * prefixed with namespace, the function name can be renamed by {@link Function} annotation.
   *
   * @since 4.2.2
   * @see Ignore
   * @see Function
   * @param namespace the functions namespace
   * @param clazz the class
   * @return the added function list.
   */
  public List<String> addStaticFunctions(final String namespace, final Class<?> clazz)
      throws IllegalAccessException, NoSuchMethodException {

    return addMethodFunctions(namespace, true, clazz);
  }

  /**
   * Import the class public methods into aviator evaluator as custom functions. The function's
   * namespace is the class name by default, and the scopes are both static and instance methods.
   * The namespace and scope can be set by {@link Import} annotation.
   *
   * @since 4.2.2
   * @see Import
   * @param clazz the class
   * @return the added function list.
   * @throws NoSuchMethodException
   * @throws IllegalAccessException
   */
  public List<String> importFunctions(final Class<?> clazz)
      throws IllegalAccessException, NoSuchMethodException {

    String namespace = clazz.getSimpleName();
    ImportScope[] scopes = {ImportScope.Static, ImportScope.Instance};

    Import importAnt = clazz.getAnnotation(Import.class);

    if (importAnt != null) {
      namespace = importAnt.ns();
      if (namespace == null || namespace.isEmpty()
          || !ExpressionParser.isJavaIdentifier(namespace)) {
        throw new IllegalArgumentException("Invalid namespace in Import annotation: " + namespace);
      }
      scopes = importAnt.scopes();
      if (scopes == null || scopes.length == 0) {
        throw new IllegalArgumentException("Empty scopes in Import annotation");
      }
    }

    List<String> result = new ArrayList<>();
    for (ImportScope scope : scopes) {
      switch (scope) {
        case Static:
          result.addAll(addStaticFunctions(namespace, clazz));
          break;
        case Instance:
          result.addAll(addInstanceFunctions(namespace, clazz));
          break;
        default:
          throw new IllegalStateException("Invalid import scope: " + scope);
      }
    }
    return result;
  }

  private Map<String, List<Method>> findMethodsFromClass(final boolean isStatic,
      final Class<?> clazz) {
    Map<String, List<Method>> methodMap = new HashMap<>();

    for (Method method : clazz.getMethods()) {
      int modifiers = method.getModifiers();
      if (Modifier.isPublic(modifiers)) {
        if (isStatic) {
          if (!Modifier.isStatic(modifiers)) {
            continue;
          }
        } else {
          if (Modifier.isStatic(modifiers)) {
            continue;
          }
        }

        if (method.getAnnotation(Ignore.class) != null) {
          continue;
        }


        String methodName = method.getName();
        Function func = method.getAnnotation(Function.class);
        if (func != null) {
          String rename = func.rename();
          if (!rename.isEmpty()) {
            if (!ExpressionParser.isJavaIdentifier(rename)) {
              throw new IllegalArgumentException("Invalid rename `" + rename + "` for method "
                  + method.getName() + " in class " + clazz);
            }
            methodName = func.rename();
          }
        }

        List<Method> methods = methodMap.get(methodName);
        if (methods == null) {
          methods = new ArrayList<>(3);
          methodMap.put(methodName, methods);
        }
        methods.add(method);
      }
    }

    return methodMap;
  }

  /**
   * Remove a function loader
   *
   * @since 4.0.0
   * @param loader
   */
  public void removeFunctionLoader(final FunctionLoader loader) {
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
  public void setOption(final Options opt, final Object val) {
    if (opt == null || val == null) {
      throw new IllegalArgumentException("Option and value should not be null.");
    }
    if (!opt.isValidValue(val)) {
      throw new IllegalArgumentException("Invalid value for option:" + opt.name());
    }
    Map<Options, Value> newOpts = new IdentityHashMap<>(this.options);
    newOpts.put(opt, opt.intoValue(val));
    this.options = newOpts;

    if (opt == Options.ENABLE_REQUIRE_LOAD_SCRIPTS) {
      if (getOptionValue(opt).bool) {
        addLoadAndRequireFunction();
      } else {
        removeLoadAndRequireFunction();
      }
    }
  }

  private void removeLoadAndRequireFunction() {
    this.removeFunction(Constants.LOAD_FN);
    this.removeFunction(Constants.REQUIRE_FN);
  }


  /**
   * Returns the current evaluator option value, returns null if missing.
   *
   * @param opt
   * @return
   */
  @Deprecated
  @SuppressWarnings("unchecked")
  public <T> T getOption(final Options opt) {
    Value val = this.options.get(opt);
    if (val == null) {
      val = opt.getDefaultValueObject();
    }

    return (T) opt.intoObject(val);
  }

  /**
   * Returns the current evaluator option value union, returns null if missing.
   *
   * @param opt
   * @return the option value, null if missing.
   */
  public Value getOptionValue(final Options opt) {
    Value val = this.options.get(opt);
    assert (val != null);
    return val;
  }


  /**
   * Returns the generated java classes byte code version, 1.6 by defualt.
   *
   * @return the bytecode version.
   */
  public int getBytecodeVersion() {
    return this.bytecodeVersion;
  }


  /**
   * Set the generated java classes java byte code version.
   *
   * @see Opcodes#V1_6
   * @param bytecodeVersion
   */
  public void setBytecodeVersion(final int bytecodeVersion) {
    this.bytecodeVersion = bytecodeVersion;
  }


  /**
   * Get the evaluator instance options
   *
   * @return
   */
  public Map<Options, Object> getOptions() {
    Map<Options, Object> ret = new HashMap<>();
    for (Map.Entry<Options, Value> entry : this.options.entrySet()) {
      ret.put(entry.getKey(), entry.getKey().intoObject(entry.getValue()));
    }
    return ret;
  }


  /**
   * Returns the functions map
   *
   * @return
   */
  public Map<String, Object> getFuncMap() {
    return this.funcMap;
  }


  /**
   * Returns the operators map.
   *
   * @return
   */
  public Map<OperatorType, AviatorFunction> getOpsMap() {
    return this.opsMap;
  }


  /**
   * Get current trace output stream,default is System.out
   *
   * @return
   */
  public OutputStream getTraceOutputStream() {
    return this.traceOutputStream;
  }


  /**
   * Set trace output stream
   *
   * @param traceOutputStream
   */
  public void setTraceOutputStream(final OutputStream traceOutputStream) {
    this.traceOutputStream = traceOutputStream;
  }

  private AviatorClassLoader initAviatorClassLoader() {
    return AccessController.doPrivileged(new PrivilegedAction<AviatorClassLoader>() {

      @Override
      public AviatorClassLoader run() {
        return new AviatorClassLoader(AviatorEvaluatorInstance.class.getClassLoader());
      }

    });
  }

  private final Map<String, Object> funcMap = new HashMap<String, Object>();

  private final ConcurrentHashMap<String/* namespace */, Object /* exports */> moduleCache =
      new ConcurrentHashMap<>();

  private final Map<OperatorType, AviatorFunction> opsMap =
      new IdentityHashMap<OperatorType, AviatorFunction>();

  private void loadModule() {
    try {
      addModule(IoModule.class);
    } catch (Exception e) {
      System.err.println("Fail to load internal modules.");
      System.exit(1);
    }
  }


  private void loadLib() {
    // Load internal functions
    // load sys lib
    addFunction(new CompareFunction());
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
    addFunction(new TupleFunction());
    addFunction(new MinFunction());
    addFunction(new MaxFunction());
    addFunction(new IdentityFunction());
    addFunction(new AssertFunction());
    addFunction(new RangeFunction());
    addFunction(new IsDefFunction());
    addFunction(new UndefFunction());
    addFunction(new TypeFunction());

    // for-loop and if statement supporting
    addFunction(new ReducerFunction());
    addFunction(new ReducerReturnFunction());
    addFunction(new ReducerContFunction());
    addFunction(new ReducerBreakFunction());
    addFunction(new IfCallccFunction());

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
    addFunction(new SeqNewArrayFunction());
    addFunction(new SeqArrayFunction());
    addFunction(new SeqNewListFunction());
    addFunction(new SeqNewMapFunction());
    addFunction(new SeqNewSetFunction());
    addFunction(new SeqMapEntryFunction());
    addFunction(new SeqIntoFunction());
    addFunction(new SeqAddFunction());
    addFunction(new SeqRemoveFunction());
    addFunction(new SeqGetFunction());
    addFunction(new SeqPutFunction());
    addFunction(new SeqMinFunction());
    addFunction(new SeqMaxFunction());
    addFunction(new SeqMapFunction());
    addFunction(new SeqReduceFunction());
    addFunction(new SeqFilterFunction());
    addFunction(new SeqSortFunction());
    addFunction(new SeqIncludeFunction());
    addFunction(new SeqContainsKeyFunction());
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

  private void addLoadAndRequireFunction() {
    // load and require
    addFunction(new RequireFunction());
    addFunction(new LoadFunction());
  }

  /**
   * Compiled Expression cache
   */
  private final ConcurrentHashMap<String/* text expression */, FutureTask<Expression>/*
                                                                                      * Compiled
                                                                                      * expression
                                                                                      * task
                                                                                      */> expressionCache =
      new ConcurrentHashMap<String, FutureTask<Expression>>();

  private LRUMap<String, FutureTask<Expression>> expressionLRUCache;



  /**
   * Create a aviator evaluator instance.
   */
  AviatorEvaluatorInstance() {
    loadLib();
    loadModule();
    addFunctionLoader(ClassPathConfigFunctionLoader.getInstance());
    for (Options opt : Options.values()) {
      this.options.put(opt, opt.getDefaultValueObject());
    }
  }

  /**
   * Use {@link LRUMap} as expression caching.It should be called when initializing the evaluator
   * instance.
   *
   * @since 5.0.0
   * @param capacity
   * @return the evaluator instance itself.
   */
  public AviatorEvaluatorInstance useLRUExpressionCache(final int capacity) {
    this.expressionLRUCache = new LRUMap<>(capacity);
    return this;
  }


  /**
   * Clear all cached compiled expression
   */
  public void clearExpressionCache() {
    resetClassLoader();
    if (this.expressionLRUCache != null) {
      synchronized (this.expressionLRUCache) {
        this.expressionLRUCache.clear();
      }
    } else {
      this.expressionCache.clear();
    }
  }

  /**
   * Reset the classloader to a new instance.
   *
   * @since 5.0.0
   */
  public void resetClassLoader() {
    this.aviatorClassLoader = initAviatorClassLoader();
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
  public AviatorClassLoader getAviatorClassLoader(final boolean cached) {
    if (cached) {
      return this.aviatorClassLoader;
    } else {
      return new AviatorClassLoader(Thread.currentThread().getContextClassLoader());
    }
  }


  /**
   * Add an aviator function,it's not thread-safe.
   *
   * @param function
   */
  public void addFunction(final AviatorFunction function) {
    final String name = function.getName();
    addFunction(name, function);
  }

  /**
   * Adds a function with the name
   *
   * @param name
   * @param function
   */
  public void addFunction(final String name, final AviatorFunction function) {
    if ("lambda".equals(name)) {
      throw new IllegalArgumentException("Invalid function name, lambda is a keyword.");
    }
    if (this.funcMap.containsKey(name)) {
      System.out.println("[Aviator WARN] The function '" + name
          + "' is already exists, but is replaced with new one.");
    }
    this.funcMap.put(name, function);
  }

  /**
   * Define a function by name and expression.
   *
   * @param name the function name
   * @param expression the expression to be executed and it's result must be a function.
   * @since 4.0.0
   */
  public void defineFunction(final String name, final String expression) {
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
  public void defineFunction(final String name, final String expression,
      final Map<String, Object> env) {
    AviatorFunction function = (AviatorFunction) this.execute(expression, env);
    this.addFunction(name, function);
  }


  /**
   * Remove an aviator function by name,it's not thread-safe.
   *
   * @param name
   * @return
   */
  public AviatorFunction removeFunction(final String name) {
    return (AviatorFunction) this.funcMap.remove(name);
  }

  /**
   * @see #getFunction(String, SymbolTable)
   * @param name
   * @return
   */
  public AviatorFunction getFunction(final String name) {
    return this.getFunction(name, null);
  }

  /**
   * Retrieve an aviator function by name,throw exception if not found or null.It's not thread-safe.
   *
   * @param name
   * @param symbolTablee
   * @return
   */
  public AviatorFunction getFunction(final String name, final SymbolTable symbolTable) {
    AviatorFunction function = (AviatorFunction) this.funcMap.get(name);
    if (function == null && this.functionLoaders != null) {
      for (FunctionLoader loader : this.functionLoaders) {
        if (loader != null) {
          function = loader.onFunctionNotFound(name);
        }
        if (function != null) {
          break;
        }
      }
    }
    if (function == null) {
      // Returns a delegate function that will try to find the function from runtime env.
      function = new RuntimeFunctionDelegator(name, symbolTable, this.functionMissing);
    }
    return function;
  }

  /**
   * Add an operator aviator function,it's not thread-safe.
   *
   * @param function
   */
  public void addOpFunction(final OperatorType opType, final AviatorFunction function) {
    this.opsMap.put(opType, function);
  }


  /**
   * Retrieve an operator aviator function by op type, return null if not found.It's not
   * thread-safe.
   *
   * @since 3.3
   * @param opType
   * @return
   */
  public AviatorFunction getOpFunction(final OperatorType opType) {
    return this.opsMap.get(opType);
  }

  /**
   * Remove an operator aviator function by op type, it's not thread-safe.
   *
   * @since 3.3
   * @param opType
   * @return
   */
  public AviatorFunction removeOpFunction(final OperatorType opType) {
    return this.opsMap.remove(opType);
  }


  /**
   * Check if the function is existed in aviator
   *
   * @param name
   * @return
   */
  public boolean containsFunction(final String name) {
    return this.funcMap.containsKey(name);
  }

  /**
   * Remove a aviator function
   *
   * @param function
   * @return
   */
  public AviatorFunction removeFunction(final AviatorFunction function) {
    return removeFunction(function.getName());
  }


  /**
   * Returns a compiled expression in cache
   *
   * @param expression
   * @return
   */
  public Expression getCachedExpression(final String expression) {
    return getCachedExpressionByKey(expression);
  }

  /**
   * Returns a compiled expression in cache by cacheKey.
   *
   * @param cacheKey
   * @return
   */
  public Expression getCachedExpressionByKey(final String cacheKey) {
    FutureTask<Expression> task = null;
    if (this.expressionLRUCache != null) {
      synchronized (this.expressionLRUCache) {
        task = this.expressionLRUCache.get(cacheKey);
      }
    } else {
      task = this.expressionCache.get(cacheKey);
    }
    if (task != null) {
      return getCompiledExpression(cacheKey, task);
    } else {
      return null;
    }
  }

  /**
   * Returns true when the expression is in cache.
   *
   * @param expression
   * @return
   * @since 4.0.0
   */
  public boolean isExpressionCached(final String expression) {
    return getCachedExpression(expression) != null;
  }

  /**
   * Returns the number of cached expressions.
   *
   * @since 4.0.0
   * @return
   */
  public int getExpressionCacheSize() {
    if (this.expressionLRUCache != null) {
      synchronized (this.expressionLRUCache) {
        return this.expressionLRUCache.size();
      }
    }
    return this.expressionCache.size();
  }


  /**
   * Compile a text expression to Expression object
   *
   * @param expression text expression
   * @param cached Whether to cache the compiled result,make true to cache it.
   * @return
   */
  public Expression compile(final String expression, final boolean cached) {
    return this.compile(expression, expression, cached);
  }

  /**
   * Compile a text expression to Expression object
   *
   * @param cacheKey unique key for caching.
   * @param expression text expression
   * @param cached Whether to cache the compiled result,make true to cache it.
   * @return
   */
  public Expression compile(final String cacheKey, final String expression, final boolean cached) {
    if (expression == null || expression.trim().length() == 0) {
      throw new CompileExpressionErrorException("Blank expression");
    }
    if (cacheKey == null || cacheKey.trim().length() == 0) {
      throw new CompileExpressionErrorException("Blank cacheKey");
    }

    if (cached) {
      FutureTask<Expression> existedTask = null;
      if (this.expressionLRUCache != null) {
        boolean runTask = false;
        synchronized (this.expressionLRUCache) {
          existedTask = this.expressionLRUCache.get(cacheKey);
          if (existedTask == null) {
            existedTask = newCompileTask(expression, cached);
            runTask = true;
            this.expressionLRUCache.put(cacheKey, existedTask);
          }
        }
        if (runTask) {
          existedTask.run();
        }
      } else {
        FutureTask<Expression> task = this.expressionCache.get(cacheKey);
        if (task != null) {
          return getCompiledExpression(expression, task);
        }
        task = newCompileTask(expression, cached);
        existedTask = this.expressionCache.putIfAbsent(cacheKey, task);
        if (existedTask == null) {
          existedTask = task;
          existedTask.run();
        }
      }
      return getCompiledExpression(cacheKey, existedTask);

    } else {
      return innerCompile(expression, cached);
    }

  }

  private FutureTask<Expression> newCompileTask(final String expression, final boolean cached) {
    return new FutureTask<>(new Callable<Expression>() {
      @Override
      public Expression call() throws Exception {
        return innerCompile(expression, cached);
      }

    });
  }


  private Expression getCompiledExpression(final String cacheKey,
      final FutureTask<Expression> task) {
    try {
      return task.get();
    } catch (Throwable t) {
      invalidateCacheByKey(cacheKey);
      final Throwable cause = t.getCause();
      if (cause instanceof ExpressionSyntaxErrorException
          || cause instanceof CompileExpressionErrorException) {
        Reflector.sneakyThrow(cause);
      }
      throw new CompileExpressionErrorException("Compile expression failure, cacheKey=" + cacheKey,
          t);
    }
  }


  private Expression innerCompile(final String expression, final boolean cached) {
    ExpressionLexer lexer = new ExpressionLexer(this, expression);
    CodeGenerator codeGenerator = newCodeGenerator(cached);
    ExpressionParser parser = new ExpressionParser(this, lexer, codeGenerator);
    Expression exp = parser.parse();
    if (getOptionValue(Options.TRACE_EVAL).bool) {
      ((BaseExpression) exp).setExpression(expression);
    }
    return exp;
  }

  private int getOptimizeLevel() {
    return getOptionValue(Options.OPTIMIZE_LEVEL).number;
  }


  public CodeGenerator newCodeGenerator(final boolean cached) {
    AviatorClassLoader classLoader = getAviatorClassLoader(cached);
    return newCodeGenerator(classLoader);

  }

  public CodeGenerator newCodeGenerator(final AviatorClassLoader classLoader) {
    switch (getOptimizeLevel()) {
      case AviatorEvaluator.COMPILE:
        ASMCodeGenerator asmCodeGenerator = new ASMCodeGenerator(this, classLoader,
            this.traceOutputStream, getOptionValue(Options.TRACE).bool);
        asmCodeGenerator.start();
        return asmCodeGenerator;
      case AviatorEvaluator.EVAL:
        return new OptimizeCodeGenerator(this, classLoader, this.traceOutputStream,
            getOptionValue(Options.TRACE).bool);
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
  public Expression compile(final String expression) {
    return compile(expression, false);
  }


  /**
   * Execute a text expression with values that are variables order in the expression.It only runs
   * in EVAL mode,and it will cache the compiled expression.It's deprecated, please use
   * {@link #execute(String, Map)} instead.
   *
   * @param expression
   * @param values
   * @return
   */
  @Deprecated
  public Object exec(final String expression, final Object... values) {
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
      throw new ExpressionNotFoundException("Null compiled expression for " + expression);
    }
  }


  /**
   * Execute a text expression with environment
   *
   * @param expression text expression
   * @param env Binding variable environment
   * @param cached Whether to cache the compiled result,make true to cache it.
   */
  public Object execute(final String expression, final Map<String, Object> env,
      final boolean cached) {
    Expression compiledExpression = compile(expression, cached);
    if (compiledExpression != null) {
      return compiledExpression.execute(env);
    } else {
      throw new ExpressionNotFoundException("Null compiled expression for " + expression);
    }
  }


  /**
   * Execute a text expression without caching
   *
   * @param expression
   * @param env
   * @return
   */
  public Object execute(final String expression, final Map<String, Object> env) {
    return execute(expression, env, false);
  }


  /**
   * Invalidate expression cache
   *
   * @param expression
   */
  public void invalidateCache(final String expression) {
    invalidateCacheByKey(expression);
  }

  /**
   * Invalidate expression cache by cacheKey
   *
   * @param cacheKey
   */
  public void invalidateCacheByKey(final String cacheKey) {
    if (this.expressionLRUCache != null) {
      synchronized (this.expressionLRUCache) {
        this.expressionLRUCache.remove(cacheKey);
      }
    } else {
      this.expressionCache.remove(cacheKey);
    }
  }


  /**
   * Execute a text expression without caching and env map.
   *
   * @param expression
   * @return
   */
  public Object execute(final String expression) {
    return execute(expression, (Map<String, Object>) null);
  }
}
