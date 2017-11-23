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


  public static void main(String[] args) {
    Foo foo = new Foo(100, 3.14f, new Date());
    Map<String, Object> env = new HashMap<String, Object>();
    env.put("foo", foo);

    String exp =
        "\"[foo i=\"+ foo.i + \", f=\" + foo.f + \", date.year=\" + (foo.date.year+1900) + \", date.month=\" + foo.date.month + \", bars[0].name=\" + #foo.bars[0].name + \"]\"";
    String result = (String) AviatorEvaluator.execute(exp, env);
    System.out.println("Execute expression: " + exp);
    System.out.println(result);

  }
}
