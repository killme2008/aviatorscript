package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;

public class TraceEvalExample {

  public static void main(String[] args) {
    AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    AviatorEvaluator.exec("a + b * c", 1, 2, 3);
    AviatorEvaluator.exec("a && b && c", true, false, true);
  }
}
