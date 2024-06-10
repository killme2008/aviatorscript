package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;

/**
 * An example to demo execution timeout
 * 
 * @author dennis
 *
 */
public class TimeoutExample {
  public static void main(String[] args) {
    AviatorEvaluator.setOption(Options.EVAL_TIMEOUT_MS, 100);
    AviatorEvaluator.execute("while(true) { }");
  }
}
