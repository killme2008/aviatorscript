package com.googlecode.aviator;

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNull;
import java.io.FileReader;
import java.io.Reader;
import javax.script.Bindings;
import javax.script.Invocable;
import javax.script.ScriptContext;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import javax.script.SimpleScriptContext;
import org.junit.Before;
import org.junit.Test;

public class ScriptEngineTest {

  final ScriptEngineManager m = new ScriptEngineManager();
  ScriptEngine engine;

  @Before
  public void setup() {
    this.engine = this.m.getEngineByName("aviator");
    assertNotNull(this.engine);
  }

  @Test
  public void testEval() throws Exception {
    assertEquals(3, this.engine.eval("1+2"));
    assertEquals(6, this.engine.eval("square = lambda(x) -> x*2 end; square(3)"));
  }

  @Test
  public void testEvalScriptFile() throws Exception {
    try (Reader reader = new FileReader("examples/function.av")) {
      assertNull(this.engine.eval(reader));
    }
  }

  @Test
  public void testScriptVariables() throws Exception {
    this.engine.put("a", 1);
    this.engine.put("b", 2);
    assertEquals(3, this.engine.eval("a+b"));
  }

  @Test
  public void testInvokeFunction() throws Exception {
    this.engine.eval("fn hello(name) {  return 'hello,' + name; }");
    Invocable inv = (Invocable) this.engine;
    assertEquals("hello,Scripting!!", inv.invokeFunction("hello", "Scripting!!"));
  }


  @Test
  public void testInvokeMethod() throws Exception {
    this.engine.eval("let obj = seq.map(); obj.hello = lambda(name) -> 'hello,'+ name end;");
    Invocable inv = (Invocable) this.engine;
    // get script object on which we want to call the method
    Object obj = this.engine.get("obj");
    assertNotNull(obj);
    assertEquals("hello,Scripting!!", inv.invokeMethod(obj, "hello", "Scripting!!"));
  }

  @Test
  public void testGetInterface1() throws Exception {
    this.engine.eval("fn run() { println('run'); }");
    Invocable inv = (Invocable) this.engine;

    // get Runnable interface object from engine. This interface methods
    // are implemented by script functions with the matching name.
    Runnable r = inv.getInterface(Runnable.class);

    r.run();
  }

  @Test
  public void testGetInterface2() throws Exception {
    this.engine.eval("let obj = seq.map(); obj.run = lambda() -> println('run'); end;");

    // get script object on which we want to implement the interface with
    Object obj = this.engine.get("obj");

    Invocable inv = (Invocable) this.engine;

    // get Runnable interface object from engine. This interface methods
    // are implemented by script methods of object 'obj'
    Runnable r = inv.getInterface(obj, Runnable.class);

    r.run();
  }

  @Test
  public void testMultiScope() throws Exception {
    this.engine.put("x", "hello");
    assertEquals("hello", this.engine.eval("identity(x)"));
    ScriptContext newContext = new SimpleScriptContext();
    Bindings engineScope = newContext.getBindings(ScriptContext.ENGINE_SCOPE);

    // add new variable "x" to the new engineScope
    engineScope.put("x", "world");

    // execute the same script - but this time pass a different script context
    assertEquals("world", this.engine.eval("identity(x)", newContext));
  }

}
