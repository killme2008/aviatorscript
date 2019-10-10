/**
 * Copyright (c) Rich Hickey.
 *
 * All rights reserved. The use and distribution terms for this software are covered by the Eclipse
 * Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php) which can be found in the
 * file epl-v10.html at the root of this distribution. By using this software in any fashion, you
 * are agreeing to be bound by the terms of this license. You must not remove this notice, or any
 * other, from this software.
 **/

/* rich Apr 19, 2006 */
package com.googlecode.aviator.utils;

import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.WeakReference;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

/**
 * Some code is copied from
 * https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
 *
 * @author boyan(boyan@antfin.com)
 *
 */
public class Reflector {

  /**
   * Throw even checked exceptions without being required to declare them or catch them. Suggested
   * idiom:
   * <p>
   * <code>throw sneakyThrow( some exception );</code>
   */
  static public RuntimeException sneakyThrow(final Throwable t) {
    // http://www.mail-archive.com/javaposse@googlegroups.com/msg05984.html
    if (t == null) {
      throw new NullPointerException();
    }
    Reflector.<RuntimeException>sneakyThrow0(t);
    return null;
  }

  @SuppressWarnings("unchecked")
  static private <T extends Throwable> void sneakyThrow0(final Throwable t) throws T {
    throw (T) t;
  }


  private static String noMethodReport(final String methodName, final Object target,
      final Object[] args) {
    return "No matching method " + methodName + " found taking " + args.length + " args"
        + (target == null ? "" : " for " + target.getClass());
  }

  static public boolean subsumes(final Class[] c1, final Class[] c2) {
    // presumes matching lengths
    Boolean better = false;
    for (int i = 0; i < c1.length; i++) {
      if (c1[i] != c2[i])// || c2[i].isPrimitive() && c1[i] == Object.class))
      {
        if (!c1[i].isPrimitive() && c2[i].isPrimitive()
            // || Number.class.isAssignableFrom(c1[i]) && c2[i].isPrimitive()
            || c2[i].isAssignableFrom(c1[i])) {
          better = true;
        } else {
          return false;
        }
      }
    }
    return better;
  }

  private static Throwable getCauseOrElse(final Exception e) {
    if (e.getCause() != null) {
      return e.getCause();
    }
    return e;
  }

  static Object invokeMatchingMethod(final String methodName, final List methods,
      final Object target, final Object[] args) {
    Method m = null;
    Object[] boxedArgs = null;
    if (methods.isEmpty()) {
      throw new IllegalArgumentException(noMethodReport(methodName, target, args));
    } else if (methods.size() == 1) {
      m = (Method) methods.get(0);
      boxedArgs = boxArgs(m.getParameterTypes(), args);
    } else // overloaded w/same arity
    {
      Method foundm = null;
      for (Iterator i = methods.iterator(); i.hasNext();) {
        m = (Method) i.next();

        Class[] params = m.getParameterTypes();
        if (isCongruent(params, args)) {
          if (foundm == null || subsumes(params, foundm.getParameterTypes())) {
            foundm = m;
            boxedArgs = boxArgs(params, args);
          }
        }
      }
      m = foundm;
    }
    if (m == null) {
      throw new IllegalArgumentException(noMethodReport(methodName, target, args));
    }

    try {
      return m.invoke(target, boxedArgs);
    } catch (Exception e) {
      throw sneakyThrow(getCauseOrElse(e));
    }

  }


  public static List<Method> getStaticMethods(final Class c, final String methodName) {
    List<Method> ret = new ArrayList<Method>();
    for (Method method : c.getMethods()) {
      int modifiers = method.getModifiers();
      if (Modifier.isStatic(modifiers) && Modifier.isPublic(modifiers)
          && methodName.equals(method.getName())) {
        ret.add(method);
      }
    }
    return ret;
  }

  static <K, V> void clearCache(final ReferenceQueue rq,
      final ConcurrentHashMap<K, Reference<V>> cache) {
    // cleanup any dead entries
    if (rq.poll() != null) {
      while (rq.poll() != null) {
        ;
      }
      for (Map.Entry<K, Reference<V>> e : cache.entrySet()) {
        Reference<V> val = e.getValue();
        if (val != null && val.get() == null) {
          cache.remove(e.getKey(), val);
        }
      }
    }
  }


  /**
   *
   * Class's instance method cache key
   *
   */
  static class MethodKey {
    Class<?> clazz;
    String name;

    public MethodKey(final Class<?> clazz, final String name) {
      super();
      this.clazz = clazz;
      this.name = name;
    }

    @Override
    public int hashCode() {
      final int prime = 31;
      int result = 1;
      result = prime * result + ((this.clazz == null) ? 0 : this.clazz.hashCode());
      result = prime * result + ((this.name == null) ? 0 : this.name.hashCode());
      return result;
    }

    @Override
    public boolean equals(final Object obj) {
      if (this == obj) {
        return true;
      }
      if (obj == null) {
        return false;
      }
      if (getClass() != obj.getClass()) {
        return false;
      }
      MethodKey other = (MethodKey) obj;
      if (this.clazz == null) {
        if (other.clazz != null) {
          return false;
        }
      } else if (!this.clazz.equals(other.clazz)) {
        return false;
      }
      if (this.name == null) {
        if (other.name != null) {
          return false;
        }
      } else if (!this.name.equals(other.name)) {
        return false;
      }
      return true;
    }

  }

  static ConcurrentHashMap<MethodKey, Reference<List<Method>>> instanceMethodsCache =
      new ConcurrentHashMap<>();
  static final ReferenceQueue<List<Method>> rq = new ReferenceQueue<>();


  public static List<Method> getInstanceMethods(final Class<?> clazz, final String methodName) {
    MethodKey key = new MethodKey(clazz, methodName);
    Reference<List<Method>> existingRef = instanceMethodsCache.get(key);
    List<Method> methods = Collections.emptyList();

    if (existingRef == null) {
      clearCache(rq, instanceMethodsCache);
      methods = getClassInstanceMethods(clazz, methodName);
      existingRef =
          instanceMethodsCache.putIfAbsent(key, new WeakReference<List<Method>>(methods, rq));
    }
    if (existingRef == null) {
      return methods;
    }

    List<Method> existingMethods = existingRef.get();
    if (existingMethods != null) {
      return existingMethods;
    }

    // entry died in the interim, do over
    instanceMethodsCache.remove(key, existingRef);
    // retry
    return getInstanceMethods(clazz, methodName);
  }

  private static List<Method> getClassInstanceMethods(final Class c, final String methodName) {
    List<Method> ret = new ArrayList<Method>();
    for (Method method : c.getMethods()) {
      int modifiers = method.getModifiers();
      if (!Modifier.isStatic(modifiers) && Modifier.isPublic(modifiers)
          && methodName.equals(method.getName())) {
        ret.add(method);
      }
    }
    return ret;
  }

  public static Object invokeStaticMethod(final Class c, final String methodName,
      final List<Method> methods, final Object[] args) {
    return invokeMatchingMethod(methodName, methods, null, args);
  }

  public static Object invokeInstanceMethod(final Class c, final String methodName,
      final Object target, final List<Method> methods, final Object[] args) {
    return invokeMatchingMethod(methodName, methods, target, args);
  }


  public static Object boxArg(final Class paramType, final Object arg) {
    if (!paramType.isPrimitive()) {
      return paramType.cast(arg);
    } else if (paramType == boolean.class) {
      return Boolean.class.cast(arg);
    } else if (paramType == char.class) {
      return Character.class.cast(arg);
    } else if (arg instanceof Number) {
      Number n = (Number) arg;
      if (paramType == int.class) {
        return n.intValue();
      } else if (paramType == float.class) {
        return n.floatValue();
      } else if (paramType == double.class) {
        return n.doubleValue();
      } else if (paramType == long.class) {
        return n.longValue();
      } else if (paramType == short.class) {
        return n.shortValue();
      } else if (paramType == byte.class) {
        return n.byteValue();
      }
    }
    throw new IllegalArgumentException(
        "Unexpected param type, expected: " + paramType + ", given: " + arg.getClass().getName());
  }

  public static Object[] boxArgs(final Class[] params, final Object[] args) {
    if (params.length == 0) {
      return null;
    }
    Object[] ret = new Object[params.length];
    for (int i = 0; i < params.length; i++) {
      Object arg = args[i];
      Class paramType = params[i];
      ret[i] = boxArg(paramType, arg);
    }
    return ret;
  }

  static public boolean paramArgTypeMatch(final Class paramType, final Class argType) {
    if (argType == null) {
      return !paramType.isPrimitive();
    }
    if (paramType == argType || paramType.isAssignableFrom(argType)) {
      return true;
    }
    if (paramType == int.class) {
      return argType == Integer.class || argType == long.class || argType == Long.class
          || argType == short.class || argType == byte.class;// || argType == FixNum.class;
    } else if (paramType == float.class) {
      return argType == Float.class || argType == double.class;
    } else if (paramType == double.class) {
      return argType == Double.class || argType == float.class;// || argType == DoubleNum.class;
    } else if (paramType == long.class) {
      return argType == Long.class || argType == int.class || argType == short.class
          || argType == byte.class;// || argType == BigNum.class;
    } else if (paramType == char.class) {
      return argType == Character.class;
    } else if (paramType == short.class) {
      return argType == Short.class;
    } else if (paramType == byte.class) {
      return argType == Byte.class;
    } else if (paramType == boolean.class) {
      return argType == Boolean.class;
    }
    return false;
  }

  static boolean isCongruent(final Class[] params, final Object[] args) {
    boolean ret = false;
    if (args == null) {
      return params.length == 0;
    }
    if (params.length == args.length) {
      ret = true;
      for (int i = 0; ret && i < params.length; i++) {
        Object arg = args[i];
        Class argType = (arg == null) ? null : arg.getClass();
        Class paramType = params[i];
        ret = paramArgTypeMatch(paramType, argType);
      }
    }
    return ret;
  }
}
