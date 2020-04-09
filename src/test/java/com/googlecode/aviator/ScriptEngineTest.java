package com.googlecode.aviator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import org.junit.Test;

public class ScriptEngineTest {

  @Test
  public void testScriptEngine() throws Exception {
    final ScriptEngineManager m = new ScriptEngineManager();
    final ScriptEngine engine = m.getEngineByName("aviator");
    assertNotNull(engine);

    assertEquals(3, engine.eval("1+2"));
    assertEquals(6, engine.eval("square = lambda(x) -> x*2 end; square(3)"));
  }
}
