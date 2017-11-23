package com.googlecode.aviator;

import com.googlecode.aviator.runtime.type.AviatorFunction;
import java.io.BufferedReader;
import java.io.Closeable;
import java.io.InputStream;
import java.io.InputStreamReader;


public class CustomFunctionLoader {

  private static String CUSTOM_FUNCTION_LIST_FILE =
      System.getenv("com.googlecode.aviator.custom_function_config_file");
  private static int tatalCustomFunctions = 0;
  static {
    if (CUSTOM_FUNCTION_LIST_FILE == null || CUSTOM_FUNCTION_LIST_FILE.trim().length() == 0) {
      CUSTOM_FUNCTION_LIST_FILE = "aviator_functions.config";
    }
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
  public static void load() {
    InputStream in = null;
    InputStreamReader inreader = null;
    BufferedReader reader = null;
    try {
      in = CustomFunctionLoader.class.getClassLoader()
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
            loadClass(line);
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
  }


  public static void loadClass(String className) {
    info("Loading custom aviator function class: '" + className + "'.");
    try {
      Class<AviatorFunction> clazz = (Class<AviatorFunction>) Class.forName(className);
      AviatorFunction func = clazz.newInstance();
      if (func != null) {
        AviatorEvaluator.addFunction(func);
        tatalCustomFunctions++;
      }
    } catch (Throwable e) {
      error("Load custom aviator function class: " + className + "' failed with error:"
          + e.getMessage() + ".");
    }
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
