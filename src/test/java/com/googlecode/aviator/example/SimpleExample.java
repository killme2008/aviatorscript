package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    // AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    Expression exp = AviatorEvaluator.getInstance()
        .compileScript(SimpleExample.class.getResource("/scripts/qsort.av").getFile());

    System.out.println(exp.execute());
    //
    // // // AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    // // System.out.println(AviatorEvaluator.execute("sum = ; t = lambda(x) -> sum = 3 end;
    // t(4)"));
    // // System.out.println(AviatorEvaluator.execute("sum = 0 ; if(true) { sum = 3 }
    // println(sum)"));
    // System.out.println(AviatorEvaluator
    // .execute("sum = 99 ; for x in range(0,10) { if(x==2) {return x;}} println(sum)"));
  }
}
