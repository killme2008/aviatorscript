package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class SeqExample {

  public static void main(String[] args) {
    int[] a = new int[10];
    for (int i = 0; i < 10; i++) {
      a[i] = i;
    }

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", a);

    System.out.println(AviatorEvaluator.execute("a[1] + 100", env));
    System.out.println(AviatorEvaluator.execute("'a[1]=' + a[1]", env));
    System.out.println(AviatorEvaluator.execute("count(a)", env));
    System.out.println(AviatorEvaluator.execute("reduce(a,+,0)", env));
    System.out.println(AviatorEvaluator.execute("seq.every(a,seq.gt(0))", env));
    System.out
        .println(AviatorEvaluator.execute("seq.every(a,seq.and(seq.ge(0), seq.lt(10)))", env));
    System.out
        .println(AviatorEvaluator.execute("seq.not_any(a,seq.and(seq.ge(0), seq.lt(10)))", env));
    System.out
        .println(AviatorEvaluator.execute("seq.not_any(a,seq.and(seq.lt(0), seq.ge(10)))", env));
    System.out.println(AviatorEvaluator.execute("seq.some(a,seq.eq(3))", env));
  }
}
