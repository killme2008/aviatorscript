package com.googlecode.aviator.script;

import javax.script.CompiledScript;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptException;
import com.googlecode.aviator.Expression;


/**
 * A compiled aviator script.
 *
 * @author libinsong1204@gmail.com
 * @author dennis(killme2008@gmail.com)
 * @date 2011-1-18 上午11:03:34
 * @version
 */
public class CompiledAviatorScript extends CompiledScript {

  private final AviatorScriptEngine engine;
  private final Expression expression;


  CompiledAviatorScript(final AviatorScriptEngine engine, final Expression expression) {
    this.engine = engine;
    this.expression = expression;
  }


  @Override
  public Object eval(final ScriptContext context) throws ScriptException {
    try {
      return this.expression.execute(context.getBindings(ScriptContext.ENGINE_SCOPE));
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }


  @Override
  public ScriptEngine getEngine() {
    return this.engine;
  }

}
