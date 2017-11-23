package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;


public class StringExample {
  public static void main(String[] args) {
    System.out.println(AviatorEvaluator.execute(" 'a\"b' "));
    System.out.println(AviatorEvaluator.execute(" \"a\'b\" "));
    System.out.println(AviatorEvaluator.execute(" 'hello '+3 "));
    System.out.println(AviatorEvaluator.execute(" 'hello '+ unknow "));
  }
}
