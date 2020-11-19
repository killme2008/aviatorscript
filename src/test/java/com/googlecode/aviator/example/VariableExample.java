package com.googlecode.aviator.example;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.AviatorEvaluator;


public class VariableExample {
  public static class Bar {
    public Bar() {
      this.name = "bar";
    }


    public String getName() {
      return this.name;
    }


    public void setName(final String name) {
      this.name = name;
    }

    private String name;

  }

  public static class Foo {

    int i;
    float f;
    Date date = new Date();
    Bar[] bars = new Bar[1];
    Map<String, Object> context = new HashMap<String, Object>();


    public Foo(final int i, final float f, final Date date) {
      super();
      this.i = i;
      this.f = f;
      this.date = date;
      this.bars[0] = new Bar();
    }


    public Map<String, Object> getContext() {
      return this.context;
    }



    public void setContext(final Map<String, Object> context) {
      this.context = context;
    }



    public Bar[] getBars() {
      return this.bars;
    }


    public void setBars(final Bar[] bars) {
      this.bars = bars;
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

  }


  public static void main(final String[] args) {
    Foo foo = new Foo(100, 3.14f, new Date());
    foo.getContext().put("99key", "99value");


    Map<String, Object> env = AviatorEvaluator.newEnv("foo", foo);

    // Syntax suger for nested variable access.
    String exp =
        "\"[foo i=#{foo.i}, f=#{foo.f}, date.year=\" + (foo.date.year+1900) + \", date.month=\" + foo.date.month + \", bars[0].name=\" + #foo.bars[0].name +\", context(99key)=\" + #`foo.context(99key)`+ \"]\"";
    String result = (String) AviatorEvaluator.execute(exp, env);
    System.out.println("Execute expression: " + exp);
    System.out.println("Result: " + result);

    // Assignment.
    exp = "#foo.bars[0].name='hello aviator' ; #foo.bars[0].name";
    result = (String) AviatorEvaluator.execute(exp, env);
    System.out.println("Execute expression: " + exp);
    System.out.println("Result: " + result);
    System.out.println(foo.bars[0].getName());

    exp = "foo.bars[0] = nil ; foo.bars[0]";
    result = (String) AviatorEvaluator.execute(exp, env);
    System.out.println("Execute expression: " + exp);
    System.out.println("Result: " + result);
    System.out.println(foo.bars[0]);
  }
}
