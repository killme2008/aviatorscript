package com.googlecode.aviator.example;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class FunctionExample {
  public static void main(String[] args) {
    System.out.println(AviatorEvaluator.execute("sysdate()"));
    System.out.println(AviatorEvaluator.execute("rand()"));
    System.out.println(AviatorEvaluator.execute("now()"));
    System.out.println(AviatorEvaluator.execute("date_to_string(sysdate(),'yyyy-MM-dd')"));
    System.out.println(AviatorEvaluator
        .execute("string_to_date(date_to_string(sysdate(),'yyyy-MM-dd'),'yyyy-MM-dd')"));

    // string function
    System.out.println("test string function...");
    System.out.println(AviatorEvaluator.execute("string.length('hello')"));
    System.out.println(AviatorEvaluator.execute("string.contains('hello','h')"));
    System.out.println(AviatorEvaluator.execute("string.startsWith('hello','h')"));
    System.out.println(AviatorEvaluator.execute("string.endsWith('hello','llo')"));
    System.out.println(
        AviatorEvaluator.execute("string.contains(\"test\",string.substring('hello',1,2))"));
    System.out.println(Arrays
        .toString((String[]) AviatorEvaluator.execute("string.split('hello world,aviator',' ')")));

    // math function
    System.out.println("test math function...");
    System.out.println(AviatorEvaluator.execute("math.abs(-3)"));
    System.out.println(AviatorEvaluator.execute("math.pow(-3,2)"));
    System.out.println(AviatorEvaluator.execute("math.sqrt(14.0)"));
    System.out.println(AviatorEvaluator.execute("math.log(100)"));
    System.out.println(AviatorEvaluator.execute("math.log10(1000)"));
    System.out.println(AviatorEvaluator.execute("math.sin(20)"));
    System.out.println(AviatorEvaluator.execute("math.cos(99.23)"));
    System.out.println(AviatorEvaluator.execute("math.tan(19.9)"));

    // seq lib
    Map<String, Object> env = new HashMap<String, Object>();
    ArrayList<Integer> list = new ArrayList<Integer>();
    list.add(3);
    list.add(100);
    list.add(-100);
    env.put("list", list);
    System.out.println(AviatorEvaluator.execute("reduce(list,+,0)", env));
    System.out.println(AviatorEvaluator.execute("filter(list,seq.exists())", env));
    System.out.println(AviatorEvaluator.execute("count(list)", env));
    System.out.println(AviatorEvaluator.execute("include(list,100)", env));
    System.out.println(AviatorEvaluator.execute("sort(list)", env));
    System.out.println(AviatorEvaluator.execute("map(list,println)", env));
    System.out.println(list);
  }

}
