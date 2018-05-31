package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.runtime.function.LambdaFunction;


public class SimpleExample {
  public static void main(String[] args) {
    AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    Map<String, Object> env = new HashMap<>();
    int[][] value = new int[10][10];
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j < 10; j++) {
        value[i][j] = i * j - 10;
        System.out.print(value[i][j] + " ");
      }
      System.out.println();
    }
    env.put("d", 100);
    env.put("x", 4);
    env.put("4", "test");
    env.put("y", 5);
    LambdaFunction s = (LambdaFunction) AviatorEvaluator.execute(
        "lambda(x) -> println(#__env__);lambda(y) -> println(#__env__);lambda(z) ->println(#__env__); x +y +z+d end end end",
        env);
    AviatorEvaluator.getInstance().getFuncMap().put("s", s);
    env.put("z", 6);

    System.out.println(AviatorEvaluator.execute("s(1)(2)(3)", env));
  }
}
