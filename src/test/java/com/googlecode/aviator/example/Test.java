package com.googlecode.aviator.example;

import java.util.HashMap;
import java.util.Map;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;

public class Test {
    public static void main(String[] args) {
        String exp =
                "(messageType=='200-trade-created-done')  && property.itemTag>nil&& string.contains(property.itemTag,'1729')";
        
        Expression expression=AviatorEvaluator.compile(exp);
        
        Map<String, String> property=new HashMap<String, String>();
       // property.put("itemTag", "2402201729");
        Map<String, Object> env=new HashMap<String, Object>();
        env.put("messageType", "200-trade-created-done");
        env.put("property", property);
        
        System.out.println(expression.execute(env));
    }

}
