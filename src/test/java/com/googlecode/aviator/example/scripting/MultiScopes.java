package com.googlecode.aviator.example.scripting;

import javax.script.Bindings;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleScriptContext;

public class MultiScopes {
  public static void main(final String[] args) throws Exception {
    ScriptEngineManager manager = new ScriptEngineManager();
    ScriptEngine engine = manager.getEngineByName("AviatorScript");

    engine.put("x", "hello");
    // print global variable "x"
    engine.eval("println(x);");
    // the above line prints "hello"

    // Now, pass a different script context
    ScriptContext newContext = new SimpleScriptContext();
    Bindings engineScope = newContext.getBindings(ScriptContext.ENGINE_SCOPE);

    // add new variable "x" to the new engineScope
    engineScope.put("x", "world");

    // execute the same script - but this time pass a different script context
    engine.eval("println(x);", newContext);
    // the above line prints "world"
  }
}
