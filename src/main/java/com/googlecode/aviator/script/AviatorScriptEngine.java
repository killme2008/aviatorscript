package com.googlecode.aviator.script;

import java.io.IOException;
import java.io.Reader;
import javax.script.AbstractScriptEngine;
import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngineFactory;
import javax.script.ScriptException;
import javax.script.SimpleBindings;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.utils.Utils;


/**
 * Aviator script engine
 *
 * @author libinsong1204@gmail.com
 * @author dennis(killme2008@gmail.com)
 * @date 2011-1-18 上午11:03:34
 * @version
 */
public class AviatorScriptEngine extends AbstractScriptEngine implements Compilable {

  private boolean cached = true;
  private final AviatorScriptEngineFactory factory;
  private final AviatorEvaluatorInstance engine;


  public AviatorScriptEngine() {
    super();
    this.factory = AviatorScriptEngineFactory.newInstance();
    this.engine = AviatorEvaluator.newInstance();
  }


  public AviatorScriptEngine(final Bindings n) {
    super(n);
    this.factory = AviatorScriptEngineFactory.newInstance();
    this.engine = AviatorEvaluator.newInstance();
  }


  public AviatorScriptEngine(final AviatorScriptEngineFactory factory) {
    this.factory = factory;
    this.engine = AviatorEvaluator.newInstance();
  }


  @Override
  public CompiledScript compile(final String script) throws ScriptException {
    return new CompiledAviatorScript(this, this.engine.compile(script, this.cached));
  }


  @Override
  public CompiledScript compile(final Reader script) throws ScriptException {
    try {
      return this.compile(Utils.readFully(script));
    } catch (IOException e) {
      throw new ScriptException(e);
    }
  }


  @Override
  public Bindings createBindings() {
    return new SimpleBindings();
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
