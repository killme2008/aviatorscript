package com.googlecode.aviator.runtime.function.system;

import java.util.Map;

import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Cast value to long
 * 
 * @author dennis
 * @Date 2011-5-18
 * @since 1.1.1
 * 
 */
public class LongFunction implements AviatorFunction {

    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length > 1) {
            throw new IllegalArgumentException("long(value) only supports one arguments");
        }
        AviatorObject arg = args[0];
        switch (arg.getAviatorType()) {
        case Boolean:
            return new AviatorLong(arg.booleanValue(env) ? 1 : 0);
        case JavaType:
            Object obj = arg.getValue(env);
            if (obj instanceof Number) {
                return new AviatorLong(((Number) obj).longValue());
            }
            else if (obj instanceof String) {
                return new AviatorLong(Long.valueOf((String) obj));
            }
            else if (obj instanceof Character) {
                return new AviatorLong(Long.valueOf(String.valueOf(obj)));
            }
            else {
                throw new ClassCastException("Could not cast " + obj.getClass().getName() + " to long");
            }
        case String:
            return new AviatorLong(Long.valueOf((String) arg.getValue(env)));
        case Number:
            return new AviatorLong(((Number) arg.getValue(env)).longValue());
        default:
            throw new ClassCastException("Could not cast " + arg + " to long");
        }
    }


    public String getName() {
        return "long";
    }

}
