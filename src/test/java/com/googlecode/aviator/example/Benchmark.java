package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;

public class Benchmark {

  public static void main(String[] args) {
    long start = System.currentTimeMillis();
    long sum = 0;
    for (int i = 0; i < 100000; i++) {
      sum += (long) AviatorEvaluator.exec("a+b*c" + i, 1, 2, 3);
    }
    System.out.println(System.currentTimeMillis() - start);
    System.out.println(sum);
  }
}
