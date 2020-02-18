package com.googlecode.aviator.example;

import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;

public class RegularExpressionExample {
  public static void main(final String[] args) {
    String email = "killme2008@gmail.com";
    Map<String, Object> env = AviatorEvaluator.newEnv("email", email);
    String username =
        (String) AviatorEvaluator.execute("email=~/([\\w0-8]+)@\\w+[\\.\\w+]+/ ? $1:'unknow'", env);
    System.out.println(username);
  }
}
