package com.googlecode.aviator.example;

import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class SimpleExample {
  public static void main(String[] args) {
    AviatorEvaluator.setOptimize(AviatorEvaluator.COMPILE);
    Map<String, Object> env = new HashMap<>();
    int[][] value = new int[10][10];
    for (int i = 0; i < 10; i++) {
      for (int j = 0; j < 10; j++) {
        value[i][j] = i * j - 10;
        System.out.print(value[i][j] + " ");
      }
      System.out.println();
    }
    env.put("a", value);
    env.put("x", 1);
    Object s = AviatorEvaluator.execute(
        "filter(a, lambda(x) -> boolean(seq.some(x, lambda(y) ->println(y+count(x)); y+count(x)>0 end)) end)",
        env);


    System.out.println(Arrays.toString((Object[]) s));
  }
}
