package com.googlecode.aviator.code.asm;

import static java.lang.invoke.MethodType.methodType;
import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.reflect.Field;
import com.googlecode.aviator.parser.AviatorClassLoader;

/**
 * A class definer
 *
 * @author dennis
 *
 */
public class ClassDefiner {

  private static final Object[] EMPTY_OBJS = new Object[] {};
  private static MethodHandle DEFINE_CLASS_HANDLE;

  static {
    // Try to get defineAnonymousClass method handle.
    try {
      Class<?> clazz = Class.forName("sun.misc.Unsafe");
      if (clazz != null) {
        Field f = clazz.getDeclaredField("theUnsafe");
        f.setAccessible(true);
        Object unsafe = f.get(null);
        MethodHandle methodHandle =
            MethodHandles.lookup().findVirtual(clazz, "defineAnonymousClass",
                methodType(Class.class, Class.class, byte[].class, Object[].class));

        if (methodHandle != null) {
          methodHandle = methodHandle.bindTo(unsafe);
        }
        DEFINE_CLASS_HANDLE = methodHandle;
      }

    } catch (Throwable e) {
      // ignore
    }
  }

  public static boolean isJDK7() {
    String version = (System.getProperty("java.version"));
    try {
      return version != null && version.startsWith("1.7");
    } catch (Throwable e) {
      return false;
    }
  }

  private static boolean preferClassLoader =
      Boolean.valueOf(System.getProperty("aviator.preferClassloaderDefiner", "false"));


  private static int errorTimes = 0;

  public static final Class<?> defineClass(String className, Class<?> clazz, byte[] bytes,
      AviatorClassLoader classLoader) throws NoSuchFieldException, IllegalAccessException {
    if (!preferClassLoader && DEFINE_CLASS_HANDLE != null) {
      try {
        Class<?> defineClass = (Class<?>) DEFINE_CLASS_HANDLE.invokeExact(clazz, bytes, EMPTY_OBJS);
        return defineClass;
      } catch (Throwable e) {
        // fallback to class loader mode.
        if (errorTimes++ > 10000) {
          preferClassLoader = true;
        }
        return defineClassByClassLoader(className, bytes, classLoader);
      }
    } else {
      return defineClassByClassLoader(className, bytes, classLoader);
    }
  }

  public static Class<?> defineClassByClassLoader(String className, byte[] bytes,
      AviatorClassLoader classLoader) {
    return classLoader.defineClass(className, bytes);
  }
}
