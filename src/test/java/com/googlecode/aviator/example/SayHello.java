package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class SayHello {
  public static void main(String[] args) {
    if (args.length < 1) {
      System.err.print("Usage: Java SayHello yourname");
      System.exit(1);
    }
    String yourname = args[0];
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("yourname", yourname);
    String result = (String) AviatorEvaluator.execute(" 'hello ' + yourname ", env);
    System.out.println(result);
  }
}
