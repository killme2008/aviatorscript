package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class SimpleExample {
  public static void main(final String[] args) throws Exception {
    Map<String, Object> env = new HashMap<String, Object>();
    String result = (String) AviatorEvaluator
        .execute("result=true;v1='test1'; if(!result) {return 'ok';} v2='test2';v2", env);
    System.out.println(result);
    System.out.println(env);
  }
}
