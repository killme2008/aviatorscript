package com.googlecode.aviator.example.scripting;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

public class RunnableImpl {
  public static void main(final String[] args) throws Exception {
    ScriptEngineManager manager = new ScriptEngineManager();
    ScriptEngine engine = manager.getEngineByName("AviatorScript");

    // AviatorScript code in a String
    String script = "fn run() { println('run called'); }";

    // evaluate script
    engine.eval(script);

    Invocable inv = (Invocable) engine;

    // get Runnable interface object from engine. This interface methods
    // are implemented by script functions with the matching name.
    Runnable r = inv.getInterface(Runnable.class);

    // start a new thread that runs the script implemented
    // runnable interface
    Thread th = new Thread(r);
    th.start();
  }
}
