package com.googlecode.aviator.example.scripting;

import javax.script.Bindings;
import javax.script.Compilable;
import javax.script.CompiledScript;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

public class CompileScript {
  public static void main(final String[] args) throws Exception {
    ScriptEngineManager manager = new ScriptEngineManager();
    ScriptEngine engine = manager.getEngineByName("AviatorScript");

    Compilable compilable = (Compilable) engine;
    CompiledScript script = compilable.compile("a + b");

    final Bindings bindings = engine.createBindings();
    bindings.put("a", 99);
    bindings.put("b", 1);
    System.out.println(script.eval(bindings));
  }

}
