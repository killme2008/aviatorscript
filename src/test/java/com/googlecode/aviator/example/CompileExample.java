package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;

public class CompileExample {
  public static void main(String[] args) {
    String expression = "a-(b-c)>100";
    // 编译表达式
    Expression compiledExp = AviatorEvaluator.compile(expression);

    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a", 100.3);
    env.put("b", 45);
    env.put("c", -199.100);

    // 执行表达式
    Boolean result = (Boolean) compiledExp.execute(env);
    System.out.println(result);
  }
}
