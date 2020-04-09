package com.googlecode.aviator.example;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.CyclicBarrier;
import java.util.concurrent.atomic.AtomicLong;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.example.VariableExample.Foo;

public class Benchmark {

  public static void main(final String[] args) throws Exception {
    // benchCompile();
    // benchCaptureGroupsOptions();
    benchmarkScript();
    // benchmarkBeanUtils();
  }

  /**
   * before optimization:benchmarkBeanUtils cost: 29064 ms. c: 1340000000
   *
   * after optimization: benchmarkBeanUtils cost: 25219 ms. c: 1340000000
   *
   * @throws Exception
   */
  private static void benchmarkBeanUtils() throws Exception {

    int threads = 200;
    final int n = 100000;

    final String exp =
        "\"[foo i=\"+ foo.i + \", f=\" + foo.f + \", date.year=\" + (foo.date.year+1900) + \", date.month=\" + foo.date.month + \", bars[0].name=\" + #foo.bars[0].name + \"]\"";

    final Foo foo = new Foo(100, 3.14f, new Date());
    final AtomicLong c = new AtomicLong();
    final CyclicBarrier barrier = new CyclicBarrier(threads + 1);

    for (int i = 0; i < threads; i++) {
      new Thread() {
        @Override
        public void run() {
          try {
            barrier.await();
            Map<String, Object> env = AviatorEvaluator.newEnv("foo", foo);
            for (int j = 0; j < n; j++) {
              String s = (String) AviatorEvaluator.execute(exp, env, true);
              // prevent JIT optimization.
              c.addAndGet(s.length());
            }
            barrier.await();
          } catch (Exception e) {
            e.printStackTrace();
          }
        }
      }.start();

    }
    long start = System.currentTimeMillis();
    barrier.await();
    barrier.await();
    System.out.println("benchmarkBeanUtils cost: " + (System.currentTimeMillis() - start) + " ms.");
    System.out.println("c: " + c.get());
  }

  private static void benchmarkScript() throws Exception {
    Expression exp = AviatorEvaluator.getInstance().compileScript("scripts/benchmark.av");

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
