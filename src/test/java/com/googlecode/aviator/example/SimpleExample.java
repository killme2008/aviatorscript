package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    Long result = (Long) AviatorEvaluator
        .execute("a=5; b=if(1==1) { println('if'); 3+a } else {println('else'); 4-a} a+b");
    System.out.println(result);
  }
}
