package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;

public class FutureAwaitExample {
  public static void main(String[] args) {
    Object future = AviatorEvaluator.execute("future(lambda ()-> 3+4 end)");
    System.out.println(future);
    System.out.println(AviatorEvaluator.exec("await(f)", future));
  }
}
