package com.googlecode.aviator;

import java.io.BufferedReader;
import java.io.Closeable;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorFunction;

/**
 * A function loader that loads function from classpath config file.
 *
 * @author dennis
 *
 */
public class ClassPathConfigFunctionLoader implements FunctionLoader {

  private static String CUSTOM_FUNCTION_LIST_FILE =
      System.getenv("com.googlecode.aviator.custom_function_config_file");
  private static int tatalCustomFunctions = 0;
  static {
    if (CUSTOM_FUNCTION_LIST_FILE == null || CUSTOM_FUNCTION_LIST_FILE.trim().length() == 0) {
      CUSTOM_FUNCTION_LIST_FILE = "aviator_functions.config";
    }
  }

  private static class StaticHolder {
    private static ClassPathConfigFunctionLoader INSTANCE = new ClassPathConfigFunctionLoader();
  }

  public static ClassPathConfigFunctionLoader getInstance() {
    return StaticHolder.INSTANCE;
  }

  private Map<String, AviatorFunction> functions = Collections.emptyMap();

  private ClassPathConfigFunctionLoader() {
    this.functions = this.load();
  }


  @Override
  public AviatorFunction onFunctionNotFound(String name) {
    return this.functions.get(name);
  }


  private static void info(String msg) {
    System.out.println("[Aviator INFO] " + msg);
  }


  private static void error(String msg) {
    System.out.println("[Aviator ERROR] " + msg);
  }


  /**
   * Load custom functions from config file, default is "aviator_functions.config" in classpath.
   *
   * @return
   */
  private Map<String, AviatorFunction> load() {
    InputStream in = null;
    InputStreamReader inreader = null;
    BufferedReader reader = null;
    Map<String, AviatorFunction> ret = new HashMap<String, AviatorFunction>();
    try {
      in = ClassPathConfigFunctionLoader.class.getClassLoader()
          .getResourceAsStream(CUSTOM_FUNCTION_LIST_FILE);
      if (in != null) {
        inreader = new InputStreamReader(in);
        reader = new BufferedReader(inreader);
        String line = null;
        while ((line = reader.readLine()) != null) {
          line = line.trim();
          if (line.startsWith("#")) {
            // skip comment
            continue;
          }
          if (line.length() > 0) {
            AviatorFunction func = loadClass(line);
            if (func != null) {
              ret.put(func.getName(), func);
            }
          }
        }
      }
    } catch (Throwable e) {
      error("Load aviator custom functions config from " + CUSTOM_FUNCTION_LIST_FILE + " failed.");
      e.printStackTrace();
    } finally {
      closeQuietly(reader);
      closeQuietly(inreader);
      closeQuietly(in);
      if (tatalCustomFunctions > 0) {
        info("Total " + tatalCustomFunctions + " custom functions loaded.");
      }
    }
    return ret;
  }


  private AviatorFunction loadClass(String className) {
    info("Loading custom aviator function class: '" + className + "'.");
    try {
      @SuppressWarnings("unchecked")
      Class<AviatorFunction> clazz = (Class<AviatorFunction>) Class.forName(className);
      AviatorFunction func = clazz.newInstance();
      if (func != null) {
        tatalCustomFunctions++;
      }
      return func;
    } catch (Throwable e) {
      error("Load custom aviator function class: " + className + "' failed with error:"
          + e.getMessage() + ".");
    }
    return null;
  }


  private static void closeQuietly(Closeable c) {
    if (c != null) {
      try {
        c.close();
      } catch (Throwable e) {
        e.printStackTrace();
      }
    }
  }
}
