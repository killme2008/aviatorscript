package com.googlecode.aviator.script;

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


/**
 * Aviator Expression engine
 *
 * @author libinsong1204@gmail.com
 * @date 2011-1-18 上午11:03:34
 * @version
 */
public class AviatorScriptEngine extends AbstractScriptEngine implements Compilable {

  // 缓存编译结果
  private boolean cached = true;
  private final AviatorScriptEngineFactory factory;
  private AviatorEvaluatorInstance evaluator;


  public AviatorScriptEngine(AviatorScriptEngineFactory factory) {
    this.factory = factory;
    this.evaluator = AviatorEvaluator.newInstance();
  }


  @Override
  public CompiledScript compile(String script) throws ScriptException {
    return new CompiledAviatorScript(this, evaluator.compile(script, this.cached));
  }


  @Override
  public CompiledScript compile(Reader script) throws ScriptException {
    throw new UnsupportedOperationException();
  }


  @Override
  public Bindings createBindings() {
    return new SimpleBindings();
  }


  @Override
  public Object eval(String script, ScriptContext context) throws ScriptException {
    return this.compile(script).eval(context);
  }


  @Override
  public Object eval(Reader reader, ScriptContext context) throws ScriptException {
    throw new UnsupportedOperationException();
  }


  @Override
  public ScriptEngineFactory getFactory() {
    return this.factory;
  }


  public boolean isCached() {
    return this.cached;
  }


  public void setCached(boolean cached) {
    this.cached = cached;
  }
}
