package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    boolean result1 = (Boolean) AviatorEvaluator.execute("1.8620639E7==18620639");
    boolean result2 = (Boolean) AviatorEvaluator.execute("1.9455127E7==19455127");
    System.out.println(result2);
  }
}
