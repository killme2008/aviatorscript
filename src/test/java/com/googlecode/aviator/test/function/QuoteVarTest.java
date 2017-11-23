package com.googlecode.aviator.test.function;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import static org.junit.Assert.assertEquals;


public class QuoteVarTest {
  public static class Bar {
    public Bar() {
      this.name = "bar";
    }


    public String getName() {
      return name;
    }


    public void setName(String name) {
      this.name = name;
    }

    private String name;

  }

  public static class Foo {

    public Bar[] getBars() {
      return bars;
    }


    public void setBars(Bar[] bars) {
      this.bars = bars;
    }

    int i;
    float f;
    Date date = new Date();
    Bar[] bars = new Bar[1];


    public Foo(int i, float f, Date date) {
      super();
      this.i = i;
      this.f = f;
      this.date = date;
      this.bars[0] = new Bar();
    }


    public int getI() {
      return i;
    }


    public void setI(int i) {
      this.i = i;
    }


    public float getF() {
      return f;
    }


    public void setF(float f) {
      this.f = f;
    }


    public Date getDate() {
      return date;
    }


    public void setDate(Date date) {
      this.date = date;
    }

  }


  @Test
  public void testQuoteVar() {
    Foo foo = new Foo(100, 3.14f, new Date());
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", foo);

    assertEquals("bar", AviatorEvaluator.execute("#foo.bars[0].name", env));
    assertEquals("hello,bar", AviatorEvaluator.execute("'hello,' + #foo.bars[0].name", env));
    assertEquals(3, AviatorEvaluator.execute("string.length(#foo.bars[0].name)", env));
  }
}
