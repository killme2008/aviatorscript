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
 * seq.array(class, ...elements) function to create a new array of special type and elements.
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
