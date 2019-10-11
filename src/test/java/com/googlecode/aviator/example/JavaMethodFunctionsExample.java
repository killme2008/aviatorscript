package com.googlecode.aviator.example;

import org.springframework.util.StringUtils;
import com.googlecode.aviator.AviatorEvaluator;

public class JavaMethodFunctionsExample {

  public static void main(final String[] args) throws Exception {
    // Import string's instance methods as custom functions under namespace 's'.
    AviatorEvaluator.addInstanceFunctions("s", String.class);
    AviatorEvaluator.execute("println(s.indexOf('hello', 'l'))");
    AviatorEvaluator.execute("println(s.replaceAll('hello', 'l', 'x'))");

    // Import StringUtils static methods as custom functions under namespace 'sutil'.
    AviatorEvaluator.addStaticFunctions("sutil", StringUtils.class);
    System.out.println(AviatorEvaluator.execute("sutil.isEmpty('hello')"));
    System.out.println(AviatorEvaluator.execute("sutil.isEmpty('')"));
  }
}
