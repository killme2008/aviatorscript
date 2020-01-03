package com.googlecode.aviator.example;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class SayHello {
  public static void main(final String[] args) {
    if (args.length < 1) {
      System.err.print("Usage: Java SayHello yourname");
      System.exit(1);
    }
    String yourname = args[0];
    Map<String, Object> env = AviatorEvaluator.newEnv("yourname", yourname);
    String result = (String) AviatorEvaluator.execute(" 'hello ' + yourname ", env);
    System.out.println(result);
  }
}
