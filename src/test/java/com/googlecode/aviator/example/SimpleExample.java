package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;


public class SimpleExample {
  public static void main(String[] args) {
    String s = (String) AviatorEvaluator.execute("'\"你好\\''");

    System.out.println(s);
  }
}
