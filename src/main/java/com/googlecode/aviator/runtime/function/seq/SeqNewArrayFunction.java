package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.TypeUtils;

/**
 * seq.array_of(class, len) function to create a new array of special type and size.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.4
 *
 */
public class SeqNewArrayFunction extends AbstractFunction {

  private static final long serialVersionUID = -6837670921285947159L;



  @Override
  public String getName() {
    return "seq.array_of";
  }



  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {

    AviatorObject clazzVar = arg1;
    Number len = FunctionUtils.getNumberValue(arg2, env);


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
        if (!name.contains(".")) {
          name = "java.lang." + name;
        }
        clazz = Class.forName(name);
      }
      return AviatorRuntimeJavaType.valueOf(Array.newInstance(clazz, len.intValue()));
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

}


