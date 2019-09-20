package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Reflector;

/**
 * seq.array function to create a new array.
 *
 * @author boyan(boyan@antfin.com)
 * @since 4.2.4
 *
 */
public class SeqNewArrayFunction extends AbstractVariadicFunction {
  @Override
  public String getName() {
    return "seq.array";
  }

  private static final Map<String, Class<?>> PRIMITIVE_TYPES = new HashMap<>();
  static {
    PRIMITIVE_TYPES.put("int", Integer.TYPE);
    PRIMITIVE_TYPES.put("long", Long.TYPE);
    PRIMITIVE_TYPES.put("double", Double.TYPE);
    PRIMITIVE_TYPES.put("float", Float.TYPE);
    PRIMITIVE_TYPES.put("bool", Boolean.TYPE);
    PRIMITIVE_TYPES.put("char", Character.TYPE);
    PRIMITIVE_TYPES.put("byte", Byte.TYPE);
    PRIMITIVE_TYPES.put("void", Void.TYPE);
    PRIMITIVE_TYPES.put("short", Short.TYPE);
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    if (args == null || args.length == 0) {
      throw new IllegalArgumentException("Missing arguments for seq.array");
    }

    AviatorObject clazzVar = args[0];


    if (clazzVar == null || clazzVar.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException(
          "Invalid class:" + (clazzVar == null ? "null" : clazzVar.desc(env)));
    }


    try {
      String name = ((AviatorJavaType) clazzVar).getName();
      Class<?> clazz = null;
      if (PRIMITIVE_TYPES.containsKey(name)) {
        clazz = PRIMITIVE_TYPES.get(name);
      } else {
        clazz = Class.forName(name);
      }
      Object ret = Array.newInstance(clazz, args.length - 1);

      for (int i = 1; i < args.length; i++) {
        Array.set(ret, i - 1, Reflector.boxArg(clazz, args[i].getValue(env)));
      }

      return new AviatorRuntimeJavaType(ret);
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

}
