package com.googlecode.aviator.example.scripting;

import javax.script.Invocable;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;

public class InvokeScriptMethod {
  public static void main(final String[] args) throws Exception {
    ScriptEngineManager manager = new ScriptEngineManager();
    ScriptEngine engine = manager.getEngineByName("AviatorScript");

    // AviatorScript code in a String. This code defines a script object 'obj'
    // with one method called 'hello'.
    String script =
        "let obj = seq.map(); obj.hello = lambda(name) -> print('Hello, ' + name); end;";
    // evaluate script
    engine.eval(script);

    // javax.script.Invocable is an optional interface.
    // Check whether your script engine implements or not!
    // Note that the JavaScript engine implements Invocable interface.
    Invocable inv = (Invocable) engine;

    // get script object on which we want to call the method
    Object obj = engine.get("obj");

    // invoke the method named "hello" on the script object "obj"
    inv.invokeMethod(obj, "hello", "Script Method !!");
  }
}

