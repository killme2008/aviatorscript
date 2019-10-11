package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;

public class JavaReflectionFunctionMissingExample {
  public static void main(final String[] args) {
    // Enable function missing based on java instance methods reflection.
    AviatorEvaluator.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
    // Calling String#indexOf by reflection
    System.out.println(AviatorEvaluator.execute("indexOf('hello world', 'w')"));
    // Calling Long#floatValue by reflection
    System.out.println(AviatorEvaluator.execute("floatValue(3)"));
    // Calling BigDecimal#add
    System.out.println(AviatorEvaluator.execute("add(3M, 4M)"));
  }
}
