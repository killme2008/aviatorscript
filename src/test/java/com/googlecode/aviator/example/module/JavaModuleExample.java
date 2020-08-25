package com.googlecode.aviator.example.module;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.annotation.Import;

/**
 * Custom java module example.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class JavaModuleExample {

  @Import(ns = "str")
  public static class StringModule {
    public static boolean isBlank(final String s) {
      return s == null || s.trim().length() == 0;
    }
  }

  public static void main(final String[] args) throws Exception {

    AviatorEvaluator.getInstance().addModule(StringModule.class);

    String script = "let str = require('str'); str.isBlank(s) ";

    System.out.println(AviatorEvaluator.execute(script, AviatorEvaluator.newEnv("s", "hello")));
    System.out.println(AviatorEvaluator.execute(script, AviatorEvaluator.newEnv("s", " ")));
    System.out.println(AviatorEvaluator.execute(script, AviatorEvaluator.newEnv("s", null)));
  }
}
