package com.googlecode.aviator.runtime.function.system;

import java.util.Map;

import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * Cast value to string
 * 
 * @author dennis
 * @Date 2011-5-18
 * @since 1.1.1
 * 
 */
public class StrFunction implements AviatorFunction {

    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length > 1) {
            throw new IllegalArgumentException("str(value) only supports one arguments");
        }
        AviatorObject arg = args[0];
        final Object value = arg.getValue(env);
        return new AviatorString(value == null ? "nil" : value.toString());
    }


    public String getName() {
        return "str";
    }

}
