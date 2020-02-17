package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.TypeUtils;

/**
 * seq.array function to create a new array.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.4
 *
 */
public class SeqArrayFunction extends AbstractVariadicFunction {
  @Override
  public String getName() {
    return "seq.array";
  }

  static {
    TypeUtils.PRIMITIVE_TYPES.put("int", Integer.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("long", Long.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("double", Double.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("float", Float.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("bool", Boolean.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("char", Character.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("byte", Byte.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("void", Void.TYPE);
    TypeUtils.PRIMITIVE_TYPES.put("short", Short.TYPE);
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
      if (TypeUtils.PRIMITIVE_TYPES.containsKey(name)) {
        clazz = TypeUtils.PRIMITIVE_TYPES.get(name);
      } else {
        clazz = Class.forName(name);
      }
      Object ret = Array.newInstance(clazz, args.length - 1);

      for (int i = 1; i < args.length; i++) {
        Array.set(ret, i - 1, Reflector.boxArg(clazz, args[i].getValue(env)));
      }

      return AviatorRuntimeJavaType.valueOf(ret);
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

}
