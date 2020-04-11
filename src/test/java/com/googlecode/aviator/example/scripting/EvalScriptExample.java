package com.googlecode.aviator.example.scripting;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

public class EvalScriptExample {
  public static void main(final String[] args) throws Exception {
    final ScriptEngineManager sem = new ScriptEngineManager();
    ScriptEngine engine = sem.getEngineByName("AviatorScript");
    engine.eval("print('Hello, World')");
  }

}
