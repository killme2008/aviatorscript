package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    AviatorEvaluator.setOptimize(AviatorEvaluator.INTERPRET);
    AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    Object result = AviatorEvaluator.execute("1==1 && 2!=2?1:2==3?4:5");
    System.out.println(result);
    System.out.println(1 == 1 && 2 != 2 ? 1 : 2 == 3 ? 4 : 5);

    // String hello = (String) AviatorEvaluator.execute("'hello,' + name",
    // AviatorEvaluator.newEnv("name", "aviator"));
    // System.out.println(hello);
    //
    // Expression exp = AviatorEvaluator.compile("map(list, lambda(v) -> v + u end)");
    // System.out.println("Uninitialized global variables: " + exp.getVariableFullNames());
    // List<Integer> list = Arrays.asList(1, 2, 3, 4, 5);
    // int u = 99;
    // System.out.println("executed: " + exp.execute(exp.newEnv("list", list, "u", u)));
  }
}
