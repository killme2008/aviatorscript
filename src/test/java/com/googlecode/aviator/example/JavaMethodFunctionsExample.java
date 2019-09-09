package com.googlecode.aviator.example;

import org.springframework.util.StringUtils;
import com.googlecode.aviator.AviatorEvaluator;

public class JavaMethodFunctionsExample {

  public static void main(final String[] args) throws Exception {
    // 导入 String 类实例方法作为自定义函数
    AviatorEvaluator.addInstanceFunctions("s", String.class);
    AviatorEvaluator.execute("println(s.indexOf('hello', 'l'))");
    AviatorEvaluator.execute("println(s.replaceAll('hello', 'l', 'x'))");

    // 导入静态方法作为自定义函数
    AviatorEvaluator.addStaticFunctions("sutil", StringUtils.class);
    System.out.println(AviatorEvaluator.execute("sutil.isEmpty('hello')"));
    System.out.println(AviatorEvaluator.execute("sutil.isEmpty('')"));
  }
}
