package com.googlecode.aviator.runtime.function;

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Method;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorNumber;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;
import com.googlecode.aviator.utils.Reflector;

/**
 * An aviator function wraps a class's static method.
 *
 * @since 4.2.2
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ClassStaticFunction extends AbstractVariadicFunction {
  private MethodHandle handle; // Only for one-arity function.
  private Class<?>[] pTypes;
  private final String name;
  private final String methodName;
  private List<Method> methods; // For reflection.
  private final Class<?> clazz;

  public ClassStaticFunction(final Class<?> clazz, final String name, final String methodName,
      final List<Method> methods) throws IllegalAccessException, NoSuchMethodException {
    this.name = name;
    this.clazz = clazz;
    this.methodName = methodName;

    if (methods.size() == 1) {
      // fast path for on e-arity function.
      this.handle = MethodHandles.lookup().unreflect(methods.get(0));
      this.pTypes = methods.get(0).getParameterTypes();
      if (this.handle == null) {
        throw new NoSuchMethodException("Method handle for " + methodName + " not found");
      }
    } else {
      // Slow path by reflections.
      this.methods = methods;
    }
  }


  @Override
  public String getName() {
    return this.name;
  }

  private static AviatorObject wrapRet(final Object ret) {
    if (ret == null) {
      return AviatorNil.NIL;
    } else if (ret instanceof Number) {
      return AviatorNumber.valueOf(ret);
    } else if (ret instanceof CharSequence) {
      return new AviatorString(ret.toString());
    } else if (ret instanceof Boolean) {
      return AviatorBoolean.valueOf((boolean) ret);
    } else if (ret instanceof AviatorObject) {
      return (AviatorObject) ret;
    } else {
      return new AviatorRuntimeJavaType(ret);
    }
  }


  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    Object[] jArgs = new Object[args.length];
    for (int i = 0; i < args.length; i++) {
      jArgs[i] = args[i].getValue(env);
    }

    if (this.handle != null) {
      try {
        return wrapRet(this.handle.invokeWithArguments(Reflector.boxArgs(this.pTypes, jArgs)));
      } catch (Throwable t) {
        throw Reflector.sneakyThrow(t);
      }
    } else {
      return wrapRet(
          Reflector.invokeStaticMethod(this.clazz, this.methodName, this.methods, jArgs));
    }
  }

}
