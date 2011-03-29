package com.googlecode.aviator.example;

import java.util.Map;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


public class AddFunctionExample {
    private static class AddFunction implements AviatorFunction {

        public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
            if (args.length != 2) {
                throw new IllegalArgumentException("Add only supports two arguments");
            }
            Number left = FunctionUtils.getNumberValue(0, args, env);
            Number right = FunctionUtils.getNumberValue(1, args, env);
            return new AviatorDouble(left.doubleValue() + right.doubleValue());
        }


        public String getName() {
            return "add";
        }

    }


    public static void main(String[] args) {
        AviatorEvaluator.addFunction(new AddFunction());
        System.out.println(AviatorEvaluator.execute("add(1,2)"));
        System.out.println(AviatorEvaluator.execute("add(add(1,2),100)"));
    }
}
