package com.googlecode.aviator.example.scripting;

import javax.script.ScriptEngine;
import javax.script.ScriptEngineManager;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.script.AviatorScriptEngine;

public class ConfigureEngine {
  public static void main(final String[] args) throws Exception {
    final ScriptEngineManager sem = new ScriptEngineManager();
    ScriptEngine engine = sem.getEngineByName("AviatorScript");
    AviatorEvaluatorInstance instance = ((AviatorScriptEngine) engine).getEngine();
    // Use compatible feature set
    instance.setOption(Options.FEATURE_SET, Feature.getCompatibleFeatures());
    // Doesn't support if in compatible feature set mode.
    engine.eval("if(true) { println('support if'); }");
  }
}
