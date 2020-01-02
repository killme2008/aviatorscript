package com.googlecode.aviator.example;

import java.util.List;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Options;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    AviatorEvaluator.setOption(Options.TRACE_EVAL, true);
    AviatorEvaluator.addInstanceFunctions("list", List.class);
    Long result = (Long) AviatorEvaluator
        .execute("list = seq.list(2,3);list.set(list, 1, 4);list[0] + list[1]");
    System.out.println(result);
  }
}
