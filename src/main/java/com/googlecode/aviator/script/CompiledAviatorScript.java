package com.googlecode.aviator.script;

import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import javax.script.Bindings;
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
      Map<String, Object> env = this.expression.newEnv();
      for (Iterator<Integer> it = context.getScopes().iterator(); it.hasNext();) {
        int scope = it.next().intValue();
        Bindings bindings = context.getBindings(scope);
        Set<String> keys = bindings.keySet();

        for (String key : keys) {
          env.put(key, bindings.get(key));
        }
      }
      return this.expression.execute(env);
    } catch (Exception e) {
      throw new ScriptException(e);
    }
  }


  @Override
  public ScriptEngine getEngine() {
    return this.engine;
  }

}
