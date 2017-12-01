package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;

public class Benchmark {

  public static void main(String[] args) {
    System.setProperty("aviator.preferClassloaderDefiner", "false");
    long start = System.currentTimeMillis();
    long sum = 0;
    for (int i = 0; i < 100000; i++) {
      Expression exp = AviatorEvaluator.compile("a+b*c" + i);
      // prevent JIT
      sum += exp.hashCode();
    }
    System.out.println(System.currentTimeMillis() - start);
    // prevent JIT
    System.out.println(sum);
  }
}
