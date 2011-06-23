package com.googlecode.aviator.runtime.function.system;

import java.util.Map;

import com.googlecode.aviator.runtime.type.AviatorDouble;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to double,double(a) eg.
 * 
 * @author dennis
 * @Date 2011-6-23
 * @since 1.1.1
 * 
 */
public class DoubleFunction implements AviatorFunction {

    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length > 1) {
            throw new IllegalArgumentException("double(value) only supports one arguments");
        }
        AviatorObject arg = args[0];
        switch (arg.getAviatorType()) {
        case Boolean:
            return new AviatorDouble(arg.booleanValue(env) ? 1 : 0);
        case JavaType:
            Object obj = arg.getValue(env);
            if (obj instanceof Number) {
                return new AviatorDouble(((Number) obj).doubleValue());
            }
            else if (obj instanceof String) {
                return new AviatorDouble(Double.parseDouble((String) obj));
            }
            else if (obj instanceof Character) {
                return new AviatorDouble(Double.parseDouble(String.valueOf(obj)));
            }
            else {
                throw new ClassCastException("Could not cast " + obj.getClass().getName() + " to double");
            }
        case String:
            return new AviatorDouble(Double.parseDouble((String) arg.getValue(env)));
        case Number:
            return new AviatorDouble(((Number) arg.getValue(env)).doubleValue());
        default:
            throw new ClassCastException("Could not cast " + arg + " to double");
        }
    }


    public String getName() {
        return "double";
    }

}
