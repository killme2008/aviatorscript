package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    Expression exp = AviatorEvaluator.getInstance().compileScript(
        "/Users/boyan/programming/java/others/aviator/src/test/java/com/googlecode/aviator/example/test.aviator");
    exp.execute();
  }
}
