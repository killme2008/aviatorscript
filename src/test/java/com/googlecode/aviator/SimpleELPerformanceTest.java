package com.googlecode.aviator;

import java.text.NumberFormat;
import java.util.HashMap;
import java.util.Map;
import junit.framework.TestCase;

public class SimpleELPerformanceTest extends TestCase {
  public void test_perf() throws Exception {

    Map<String, Object> ctx = new HashMap<String, Object>();
    ctx.put("a", 3);
    ctx.put("b", 4);
    ctx.put("c", 5);

    // AviatorEvaluator.setTrace(true);
    for (int i = 0; i < 10; ++i) {
      perf(ctx);
    }
  }

  private void perf(Map<String, Object> ctx) {
    Expression exp = AviatorEvaluator.compile("(a+b)*c");
    long startMillis = System.currentTimeMillis();

    final int COUNT = 1000 * 1000;
    for (int i = 0; i < COUNT; ++i) {
      exp.execute(ctx);
    }

    long millis = System.currentTimeMillis() - startMillis;

    System.out.println("time : " + NumberFormat.getInstance().format(millis));
  }
}
