package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;

public class LambdaExample {

  public static void main(String[] args) {

    String exp = "a=1; b = lambda(x) -> a+ x end ; a=4 ; b(5)";
    System.out.println(AviatorEvaluator.execute(exp)); // output 6


  }
}
