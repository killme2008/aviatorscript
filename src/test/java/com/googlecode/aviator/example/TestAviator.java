package com.googlecode.aviator.example;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;

public class TestAviator {
  int i;
  float f;
  Date date;

  public TestAviator(final int i, final float f, final Date date) {
    this.i = i;
    this.f = f;
    this.date = date;
  }

  public int getI() {
    return this.i;
  }

  public void setI(final int i) {
    this.i = i;
  }

  public float getF() {
    return this.f;
  }

  public void setF(final float f) {
    this.f = f;
  }

  public Date getDate() {
    return this.date;
  }

  public void setDate(final Date date) {
    this.date = date;
  }


  public static void main(final String[] args) {
    TestAviator foo = new TestAviator(100, 3.14f, new Date());
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", foo);
    System.out.println(AviatorEvaluator.execute("'foo.i = '+foo.i", env));
    System.out.println(AviatorEvaluator.execute("'foo.f = '+foo.f", env));
    System.out.println(AviatorEvaluator.execute("'foo.date.year = '+ (foo.date.year+1990)", env));
    System.out.println(AviatorEvaluator.execute("foo.i = 100; foo.f + foo.i", env));
  }
}
