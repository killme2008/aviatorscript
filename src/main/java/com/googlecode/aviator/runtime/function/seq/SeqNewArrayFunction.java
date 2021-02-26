package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;
import com.googlecode.aviator.utils.TypeUtils;

/**
 * seq.array_of(class, len) function to create a new array of special type and size.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.4
 *
 */
public class SeqNewArrayFunction extends AbstractVariadicFunction {

  private static final long serialVersionUID = -6837670921285947159L;



  @Override
  public String getName() {
    return "seq.array_of";
  }


  private Class<?> getElementClass(final Map<String, Object> env, final AviatorObject arg1) {
    AviatorObject clazzVar = arg1;
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
        assert (env instanceof Env);
        clazz = ((Env) env).resolveClassSymbol(name);
      }
      return clazz;
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {

    try {
      Class<?> clazz = getElementClass(env, arg1);
      Number len = FunctionUtils.getNumberValue(arg2, env);
      return AviatorRuntimeJavaType.valueOf(Array.newInstance(clazz, len.intValue()));
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {
    try {
      Class<?> clazz = getElementClass(env, arg1);
      Number len1 = FunctionUtils.getNumberValue(arg2, env);
      Number len2 = FunctionUtils.getNumberValue(arg3, env);

      return AviatorRuntimeJavaType
          .valueOf(Array.newInstance(clazz, len1.intValue(), len2.intValue()));
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4) {
    try {
      Class<?> clazz = getElementClass(env, arg1);
      Number len1 = FunctionUtils.getNumberValue(arg2, env);
      Number len2 = FunctionUtils.getNumberValue(arg3, env);
      Number len3 = FunctionUtils.getNumberValue(arg4, env);

      return AviatorRuntimeJavaType
          .valueOf(Array.newInstance(clazz, len1.intValue(), len2.intValue(), len3.intValue()));
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }


  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    if (args.length < 4) {
      throw new IllegalArgumentException(
          "Wrong number of args(" + args.length + ") passed to: " + getName());
    }
    try {
      Class<?> clazz = getElementClass(env, args[0]);
      int[] lens = new int[args.length - 1];
      for (int i = 1; i < args.length; i++) {
        Number len = FunctionUtils.getNumberValue(args[i], env);
        lens[i - 1] = len.intValue();
      }
      return AviatorRuntimeJavaType.valueOf(Array.newInstance(clazz, lens));
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }


}


