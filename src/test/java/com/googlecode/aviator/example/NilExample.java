package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;


public class NilExample {
  public static void main(String[] args) {

    System.out.println(AviatorEvaluator.execute("nil == nil"));
    System.out.println(AviatorEvaluator.execute(" 3> nil"));
    System.out.println(AviatorEvaluator.execute(" ' '>nil "));
    System.out.println(AviatorEvaluator.execute(" a==nil "));
    System.out.println(AviatorEvaluator.execute(" 1!=nil "));
    System.out.println(AviatorEvaluator.execute(" nil<='hello' "));
  }
}
