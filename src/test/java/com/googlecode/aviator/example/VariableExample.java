package com.googlecode.aviator.example;

import java.util.Date;
import java.util.HashMap;
import java.util.Map;

import com.googlecode.aviator.AviatorEvaluator;


public class VariableExample {
    public static class Foo {
        int i;
        float f;
        Date date = new Date();


        public Foo(int i, float f, Date date) {
            super();
            this.i = i;
            this.f = f;
            this.date = date;
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

        String result =
                (String) AviatorEvaluator.execute(
                    " '[foo i='+ foo.i + ' f='+foo.f+' year='+(foo.date.year+1900)+ ' month='+foo.date.month +']' ",
                    env);
        System.out.println(result);

    }
}
