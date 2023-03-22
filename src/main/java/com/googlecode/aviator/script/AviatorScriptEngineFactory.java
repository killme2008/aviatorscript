package com.googlecode.aviator.script;

import java.util.Arrays;
import java.util.Collections;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import javax.script.ScriptEngine;
import javax.script.ScriptEngineFactory;
import com.googlecode.aviator.AviatorEvaluator;


/**
 * Aviator script engine factory.
 *
 * @author libinsong1204@gmail.com
 * @author dennis
 * @date 2011-1-18 上午11:03:34
 * @version
 */
public class AviatorScriptEngineFactory implements ScriptEngineFactory {

  private static final List<String> extensions =
      Collections.unmodifiableList(Arrays.asList("av", "aviator"));
  private static final List<String> mimeTypes =
      Collections.unmodifiableList(Arrays.asList("text/aviator", "text/aviatorscript"));
  private static final List<String> names = Collections
      .unmodifiableList(Arrays.asList("Aviator", "aviator", "aviatorscript", "AviatorScript"));

  private static final Map<String, String> PARAM_MAP = new HashMap<String, String>();
  static {
    PARAM_MAP.put(ScriptEngine.ENGINE, "Aviator");
    PARAM_MAP.put(ScriptEngine.ENGINE_VERSION, AviatorEvaluator.VERSION);
    PARAM_MAP.put(ScriptEngine.LANGUAGE, "AviatorScript");
    PARAM_MAP.put(ScriptEngine.LANGUAGE_VERSION, AviatorEvaluator.VERSION);
  }


  public static final AviatorScriptEngineFactory newInstance() {
    return new AviatorScriptEngineFactory();
  }


  @Override
  public String getEngineName() {
    return PARAM_MAP.get(ScriptEngine.ENGINE);
  }


  @Override
  public String getEngineVersion() {
    return PARAM_MAP.get(ScriptEngine.ENGINE_VERSION);
  }


  @Override
  public List<String> getExtensions() {
    return extensions;
  }


  @Override
  public String getLanguageName() {
    return PARAM_MAP.get(ScriptEngine.LANGUAGE);
  }


  @Override
  public String getLanguageVersion() {
    return PARAM_MAP.get(ScriptEngine.LANGUAGE_VERSION);
  }


  @Override
  public String getMethodCallSyntax(final String obj, final String m, final String... args) {
    StringBuilder sb = new StringBuilder(m);
    sb.append("(").append(obj);
    if (args != null) {
      for (String s : args) {
        sb.append(",").append(s);
      }
    }
    sb.append(")");
    return sb.toString();

  }


  @Override
  public List<String> getMimeTypes() {
    return mimeTypes;
  }


  @Override
  public List<String> getNames() {
    return names;
  }


  @Override
  public String getOutputStatement(final String toDisplay) {
    return "print(+" + toDisplay + ")";
  }


  @Override
  public Object getParameter(final String key) {
    return PARAM_MAP.get(key);
  }


  @Override
  public String getProgram(final String... statements) {
    StringBuilder sb = new StringBuilder();
    for (String stmt : statements) {
      sb.append(stmt).append(";");
    }
    return sb.toString();
  }


  @Override
  public ScriptEngine getScriptEngine() {
    return new AviatorScriptEngine(this);
  }

}
