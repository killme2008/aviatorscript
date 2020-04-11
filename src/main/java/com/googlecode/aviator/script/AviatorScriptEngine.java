package com.googlecode.aviator.script;

import java.io.IOException;
import java.io.Reader;
import java.lang.reflect.InvocationHandler;
import java.lang.reflect.Method;
import java.lang.reflect.Proxy;
import java.util.Map;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;
import com.googlecode.aviator.utils.Utils;
import com.googlecode.aviator.utils.VarNameGenerator;


/**
 * Aviator script engine
 *
 * @author libinsong1204@gmail.com
 * @author dennis(killme2008@gmail.com)
 * @date 2011-1-18 上午11:03:34
 * @version
 */
public class AviatorScriptEngine extends AbstractScriptEngine implements Compilable, Invocable {

  private static final int KEY_THRESHOLD = 4096;
  private boolean cached = true;
  private final AviatorScriptEngineFactory factory;
  private final AviatorEvaluatorInstance engine;

  public static final ThreadLocal<VarNameGenerator> TEMP_VAR_GEN =
      new ThreadLocal<VarNameGenerator>() {

        @Override
        protected VarNameGenerator initialValue() {
          return new VarNameGenerator();
        }

      };

  public AviatorScriptEngine() {
    super();
    this.factory = AviatorScriptEngineFactory.newInstance();
    this.engine = AviatorEvaluator.newInstance();
    this.engine.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
  }



  @SuppressWarnings("unchecked")
  @Override
  public Object invokeMethod(final Object thiz, final String name, final Object... args)
      throws ScriptException, NoSuchMethodException {
    final AviatorBindings bindings =
        (AviatorBindings) createBindings(this.context.getBindings(ScriptContext.ENGINE_SCOPE));
    bindings.setmOverrides((Map<String, Object>) thiz);
    return invokeFunction(name, createBindings(bindings), args);
  }



  public AviatorEvaluatorInstance getEngine() {
    return this.engine;
  }



  private String genVar() {
    return TEMP_VAR_GEN.get().gen();
  }


  @Override
  public Object invokeFunction(final String name, final Object... args)
      throws ScriptException, NoSuchMethodException {
    final Bindings bindings = createBindings(this.context.getBindings(ScriptContext.ENGINE_SCOPE));

    return invokeFunction(name, bindings, args);

  }



  private Object invokeFunction(final String name, final Bindings bindings, final Object... args)
      throws ScriptException {
    StringBuilder sb = new StringBuilder(name);
    sb.append("(");
    if (args != null) {
      boolean wasFirst = true;
      for (Object arg : args) {
        String var = genVar();
        bindings.put(var, arg);
        if (wasFirst) {
          sb.append(var);
          wasFirst = false;
        } else {
          sb.append(",").append(var);
        }
      }
    }
    sb.append(")");

    return eval(sb.toString(), bindings);
  }

  private class DynamicFunctionInvocationHandler implements InvocationHandler {

    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args)
        throws Throwable {
      String name = method.getName();
      return invokeFunction(name, args);
    }
  }

  private final DynamicFunctionInvocationHandler functionProxyHandler =
      new DynamicFunctionInvocationHandler();

  @SuppressWarnings("unchecked")
  @Override
  public <T> T getInterface(final Class<T> clasz) {
    return (T) Proxy.newProxyInstance(clasz.getClassLoader(), new Class[] {clasz},
        this.functionProxyHandler);
  }

  private class DynamicMethodInvocationHandler implements InvocationHandler {

    private final Object thiz;


    public DynamicMethodInvocationHandler(final Object thiz) {
      super();
      this.thiz = thiz;
    }


    @Override
    public Object invoke(final Object proxy, final Method method, final Object[] args)
        throws Throwable {
      String name = method.getName();
      return invokeMethod(this.thiz, name, args);
    }
  }


  @SuppressWarnings("unchecked")
  @Override
  public <T> T getInterface(final Object thiz, final Class<T> clasz) {
    return (T) Proxy.newProxyInstance(clasz.getClassLoader(), new Class[] {clasz},
        new DynamicMethodInvocationHandler(thiz));
  }



  public AviatorScriptEngine(final Bindings n) {
    super(n);
    this.factory = AviatorScriptEngineFactory.newInstance();
    this.engine = AviatorEvaluator.newInstance();
  }


  public AviatorScriptEngine(final AviatorScriptEngineFactory factory) {
    this.factory = factory;
    this.engine = AviatorEvaluator.newInstance();
    this.engine.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
  }


  @Override
  public CompiledScript compile(final String script) throws ScriptException {
    return new CompiledAviatorScript(this,
        this.engine.compile(getCachingKey(script), script, this.cached));
  }


  public String getCachingKey(final String script) {
    if (script.length() < KEY_THRESHOLD) {
      return script;
    } else {
      return Utils.md5sum(script);
    }
  }


  @Override
  public CompiledScript compile(final Reader script) throws ScriptException {
    try {
      return this.compile(Utils.readFully(script));
    } catch (IOException e) {
      throw new ScriptException(e);
    }
  }


  public Bindings createBindings(final Bindings parent) {
    final AviatorBindings bindings = new AviatorBindings(parent);
    bindings.setInstance(this.engine);
    return bindings;
  }


  @Override
  public Bindings createBindings() {
    final AviatorBindings bindings = new AviatorBindings();
    bindings.setInstance(this.engine);
    return bindings;
  }

  @Override
  public Object eval(final String script, final ScriptContext context) throws ScriptException {
    return this.compile(script).eval(context);
  }


  @Override
  public Object eval(final Reader reader, final ScriptContext context) throws ScriptException {
    try {
      return eval(Utils.readFully(reader), context);
    } catch (IOException e) {
      throw new ScriptException(e);
    }
  }


  @Override
  public ScriptEngineFactory getFactory() {
    return this.factory;
  }


  public boolean isCached() {
    return this.cached;
  }


  /**
   * Setting whether to cache the compiled script, default is true(caching).
   *
   * @param cached true means enable caching.
   */
  public void setCached(final boolean cached) {
    this.cached = cached;
  }
}
