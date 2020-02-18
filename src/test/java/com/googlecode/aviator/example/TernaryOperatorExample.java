package com.googlecode.aviator.example;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class TernaryOperatorExample {
  public static void main(final String[] args) {
    if (args.length < 1) {
      System.err.println("Usage: java TernaryOperatorExample [number]");
      System.exit(1);
    }
    int num = Integer.parseInt(args[0]);
    Map<String, Object> env = AviatorEvaluator.newEnv("a", num);
    String result = (String) AviatorEvaluator.execute("a>0? 'yes':'no'", env);
    System.out.println(result);
  }
}
