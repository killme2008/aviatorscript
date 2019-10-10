package com.googlecode.aviator.runtime;

import java.util.Map;
import com.googlecode.aviator.FunctionMissing;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Reflector;

/**
 * A function missing implementation that invoking first argument class's java instance method by
 * reflection with all arguments(the first argument as `this` pointer).
 *
 * @since 4.2.5
 *
 * @author dennis zhuang(killme2008@gmail.com)
 *
 */
public class JavaMethodReflectionFunctionMissing implements FunctionMissing {

  private JavaMethodReflectionFunctionMissing() {

  }

  private static final JavaMethodReflectionFunctionMissing INSTANCE =
      new JavaMethodReflectionFunctionMissing();

  /**
   * Retrieve a global singleton JavaMethodReflectionFunctionMissing instance.
   *
   * @return
   */
  public static JavaMethodReflectionFunctionMissing getInstance() {
    return INSTANCE;
  }

  @Override
  public AviatorObject onFunctionMissing(final String name, final Map<String, Object> env,
      final AviatorObject... args) {

    if (args == null || args.length < 1) {
      throw new FunctionNotFoundException(
          "Function not found: " + name + ", could not resolve with no arguments");
    }

    Object firstArg = args[0].getValue(env);

    if (firstArg == null) {
      throw new FunctionNotFoundException(
          "Function not found: " + name + ", the first argument is null");
    }

    Class<?> clazz = firstArg.getClass();


    Object[] jArgs = new Object[args.length - 1];
    for (int i = 1; i < args.length; i++) {
      jArgs[i - 1] = args[i].getValue(env);
    }

    return FunctionUtils.wrapReturn(Reflector.invokeInstanceMethod(clazz, name, firstArg,
        Reflector.getInstanceMethods(clazz, name), jArgs));
  }

}
