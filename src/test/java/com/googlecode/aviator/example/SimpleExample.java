package com.googlecode.aviator.example;

import java.util.Arrays;
import java.util.List;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    Long result = (Long) AviatorEvaluator.execute("1+2+3");
    System.out.println(result);

    String hello = (String) AviatorEvaluator.execute("'hello,' + name",
        AviatorEvaluator.newEnv("name", "aviator"));
    System.out.println(hello);

    Expression exp = AviatorEvaluator.compile("map(list, lambda(v) -> v + u end)");
    System.out.println("Uninitialized global variables: " + exp.getVariableFullNames());
    List<Integer> list = Arrays.asList(1, 2, 3, 4, 5);
    int u = 99;
    System.out.println("executed: " + exp.execute(exp.newEnv("list", list, "u", u)));
  }
}
