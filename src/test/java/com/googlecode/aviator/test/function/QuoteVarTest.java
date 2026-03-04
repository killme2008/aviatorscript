package com.googlecode.aviator.test.function;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.exception.ExpressionSyntaxErrorException;
import static com.googlecode.aviator.TestUtils.assertEquals;


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


  @Test
  public void testPropertyAccessWithoutQuote() {
    Foo foo = new Foo(100, 3.14f, new Date());
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", foo);

    // These should work WITHOUT # prefix
    assertEquals("bar", AviatorEvaluator.execute("foo.bars[0].name", env));
    assertEquals("hello,bar", AviatorEvaluator.execute("'hello,' + foo.bars[0].name", env));
    assertEquals(3, AviatorEvaluator.execute("string.length(foo.bars[0].name)", env));
    assertEquals(100, AviatorEvaluator.execute("foo.i", env));
    assertEquals(3.14f, AviatorEvaluator.execute("foo.f", env));
  }


  @Test
  public void testFullKeyPriority() {
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("a.b.c", "full-key-value");

    Map<String, Object> innerB = new HashMap<String, Object>();
    innerB.put("c", "property-chain-value");
    Map<String, Object> innerA = new HashMap<String, Object>();
    innerA.put("b", innerB);
    env.put("a", innerA);

    // Full key should take priority
    assertEquals("full-key-value", AviatorEvaluator.execute("a.b.c", env));
  }


  @Test
  public void testPropertyChainWhenNoFullKey() {
    Map<String, Object> env = new HashMap<String, Object>();

    Map<String, Object> innerB = new HashMap<String, Object>();
    innerB.put("c", "property-chain-value");
    Map<String, Object> innerA = new HashMap<String, Object>();
    innerA.put("b", innerB);
    env.put("a", innerA);

    // When "a.b.c" key doesn't exist, should traverse property chain
    assertEquals("property-chain-value", AviatorEvaluator.execute("a.b.c", env));
  }


  @Test(expected = ExpressionSyntaxErrorException.class)
  public void testInvalidPropertyChainWithEmptySegment() {
    AviatorEvaluator.execute("a..b");
  }
}
