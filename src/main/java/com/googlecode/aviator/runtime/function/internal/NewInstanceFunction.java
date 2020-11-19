package com.googlecode.aviator.runtime.function.internal;

import java.lang.reflect.Constructor;
import java.util.Arrays;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;

/**
 * __new(Class, ...args) to create an instance of special class with arguments.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class NewInstanceFunction extends AbstractVariadicFunction {

  private static final long serialVersionUID = -2257891325568093945L;

  private NewInstanceFunction() {

  }

  public static final NewInstanceFunction INSTANCE = new NewInstanceFunction();

  @Override
  public String getName() {
    return "__new";
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    if (args == null || args.length == 0) {
      throw new IllegalArgumentException("Missing className for new");
    }
    AviatorObject firstArg = args[0];
    if (firstArg.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException("Invalid class name: " + firstArg.desc(env));
    }
    String className = ((AviatorJavaType) firstArg).getName();

    try {
      assert (env instanceof Env);
      Class<?> clazz = ((Env) env).resolveClassSymbol(className);
      Constructor<?>[] constructors = clazz.getConstructors();
      final Object[] constructArgs = new Object[args.length - 1];
      for (int i = 1; i < args.length; i++) {
        constructArgs[i - 1] = args[i].getValue(env);
      }
      Constructor<?> bestMatch = null;
      for (Constructor<?> constructor : constructors) {
        final Class<?>[] pTypes = constructor.getParameterTypes();
        if (pTypes.length == constructArgs.length) {
          if (Reflector.isCongruent(pTypes, constructArgs)) {
            bestMatch = constructor;
            for (int i = 0; i < constructArgs.length; i++) {
              constructArgs[i] = Reflector.boxArg(pTypes[i], constructArgs[i]);
            }
            break;
          }
        }
      }

      if (bestMatch == null) {
        throw new IllegalStateException("Could not find constructor for class " + className
            + " with arguments: " + Arrays.toString(constructArgs));
      }

      return AviatorRuntimeJavaType.valueOf(bestMatch.newInstance(constructArgs));
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

}
