package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;


public class AddFunctionExample {
  public static void main(String[] args) {
    AviatorEvaluator.addFunction(new AddFunction());
    System.out.println(AviatorEvaluator.execute("add(1,2)"));
    System.out.println(AviatorEvaluator.execute("add(add(1,2),100)"));
  }
}
