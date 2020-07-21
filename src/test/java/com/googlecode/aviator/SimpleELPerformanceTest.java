package com.googlecode.aviator;

import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Map;
import junit.framework.TestCase;

public class SimpleELPerformanceTest extends TestCase {
  public void test_perf() throws Exception {

    // AviatorEvaluator.setTrace(true);
    for (int i = 0; i < 10; ++i) {
      perf();
      perfVarAccess();
      perfStringInterpolation();
    }
  }

  private void perfStringInterpolation() {
    System.out.println("Perf string interpolation");
    final String script = "'a=#{a}, b=#{b}, c=#{c}, #{a}+#{b}+#{c}=#{a+b+c}'";
    // final String script =
    // "'a=' + a + ', b='+b +', c=' +c +', ' + a + '+' + b + '+' + c + '=' + (a+b+c)";
    Expression exp = AviatorEvaluator.compile(script);
    Map<String, Object> ctx = exp.newEnv("a", 100, "b", 100, "c", 200);

    long startMillis = System.currentTimeMillis();

    final int COUNT = 1 * 1000 * 1000;
    for (int i = 0; i < COUNT; ++i) {
      if (!"a=100, b=100, c=200, 100+100+200=400".equals(exp.execute(ctx))) {
        fail();
      }
    }

    long millis = System.currentTimeMillis() - startMillis;

    System.out.println("time : " + NumberFormat.getInstance().format(millis));
  }

  private void perfVarAccess() {
    System.out.println("Perf a.b.c");
    Expression exp = AviatorEvaluator.compile("a.b.c");
    final HashMap<String, Object> a = new HashMap<>();
    final HashMap<Object, Object> b = new HashMap<>();
    b.put("c", 100);
    a.put("b", b);
    Map<String, Object> ctx = exp.newEnv("a", a);

    long startMillis = System.currentTimeMillis();

    final int COUNT = 10000 * 1000;
    for (int i = 0; i < COUNT; ++i) {
      exp.execute(ctx);
    }

    long millis = System.currentTimeMillis() - startMillis;

    System.out.println("time : " + NumberFormat.getInstance().format(millis));
  }

  private void perf() {
    System.out.println("Perf (a+b+100)*c*100");
    Expression exp = AviatorEvaluator.compile("(a+b+100)*c*100");
    Map<String, Object> ctx = exp.newEnv("a", 1001, "b", 4, "c", 5);

    long startMillis = System.currentTimeMillis();

    final int COUNT = 10000 * 1000;
    for (int i = 0; i < COUNT; ++i) {
      exp.execute(ctx);
    }

    long millis = System.currentTimeMillis() - startMillis;

    System.out.println("time : " + NumberFormat.getInstance().format(millis));
  }
}
