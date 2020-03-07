package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;

public class Benchmark {

  public static void main(final String[] args) throws Exception {
    // benchCompile();
    // benchCaptureGroupsOptions();
    benchmarkScript();
  }

  private static void benchmarkScript() throws Exception {
    Expression exp = AviatorEvaluator.getInstance()
        .compileScript(SimpleExample.class.getResource("/scripts/benchmark.av").getFile());

    System.out.println(exp.execute());
  }

  private static void benchCaptureGroupsOptions() {
    AviatorEvaluator.setOption(Options.PUT_CAPTURING_GROUPS_INTO_ENV, false);
    long sum = 0;
    Map<String, Object> env = new HashMap<String, Object>();


    long start = System.currentTimeMillis();
    for (int i = 0; i < 100000; i++) {
      env.put("email", "aviator" + i + "@gmail.com");
      long a = (Long) AviatorEvaluator.execute("email=~/([\\w0-8]+)@\\w+[\\.\\w+]+/ ? 99:-1", env);
      // prevent JIT
      sum += a;
    }
    System.out.println(System.currentTimeMillis() - start);
    // prevent JIT
    System.out.println(sum + " " + env);
  }

  private static void benchCompile() {
    System.setProperty("aviator.preferClassloaderDefiner", "false");
    // System.setProperty("aviator.preferClassloaderDefiner", "true");
    long start = System.currentTimeMillis();
    long sum = 0;
    for (int i = 0; i < 100000; i++) {
      Expression exp = AviatorEvaluator.compile("a+b*c" + i);
      // prevent JIT
      sum += exp.hashCode();
    }
    System.out.println(System.currentTimeMillis() - start);
    // prevent JIT
    System.out.println(sum);
  }
}
