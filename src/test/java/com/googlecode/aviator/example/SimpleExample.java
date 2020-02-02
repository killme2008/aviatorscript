package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    // AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    Expression exp = AviatorEvaluator.getInstance()
        .compileScript(SimpleExample.class.getResource("/test.aviator").getFile());

    System.out.println(exp.execute());
  }
}
