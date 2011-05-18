package com.googlecode.aviator.script;

import static org.junit.Assert.*;

import javax.script.ScriptEngineManager;

import org.junit.Ignore;


public class AviatorEngineUnitTest {
    private final ScriptEngineManager scriptEngineManager = new ScriptEngineManager(Thread.currentThread().getContextClassLoader());


    @Ignore
    public void getScriptEngineByName() {
       
        AviatorScriptEngine engine = (AviatorScriptEngine)this.scriptEngineManager.getEngineByName("Aviator");
        assertNotNull(engine);
    }
}
