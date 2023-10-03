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

import java.lang.invoke.MethodHandle;
import java.lang.invoke.MethodHandles;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.lang.ref.SoftReference;
import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.annotation.Function;
import com.googlecode.aviator.annotation.Ignore;
import com.googlecode.aviator.exception.NoSuchPropertyException;
import com.googlecode.aviator.parser.ExpressionParser;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.ClassMethodFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;

/**
 * Some code is copied from
 * https://github.com/clojure/clojure/blob/master/src/jvm/clojure/lang/Reflector.java
 *
 * @author dennis(killme2008@gmail.com)
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

  static public boolean subsumes(final Class<?>[] c1, final Class<?>[] c2) {
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


  public static StringBuilder capitalize(final StringBuilder sb, final String s) {
    if (s == null) {
      return sb;
    }
    if (s.length() > 1 && Character.isUpperCase(s.charAt(1))) {
      return sb.append(s);
    }
    sb.append(s.substring(0, 1).toUpperCase());
    sb.append(s.substring(1));
    return sb;
  }

  static class PropertyFoundResult {
    MethodHandle handle;
    boolean isBooleanType;
    ClassMethodFunction func;



    public PropertyFoundResult(final ClassMethodFunction func) {
      super();
      this.func = func;
    }

    public PropertyFoundResult(final MethodHandle handle, final boolean isBooleanType) {
      super();
      this.isBooleanType = isBooleanType;
      this.handle = handle;
    }

    @Override
    public String toString() {
      return "MethodHandleResult [handle=" + this.handle + ", isBooleanType=" + this.isBooleanType
          + ", func=" + this.func + "]";
    }
  }

  /**
   * static and instance fields property caching
   */
  public static ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>> cachedProperties =
      new ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>>();

  /**
   * instance fields setter caching.
   */
  public static ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>> cachedSettters =
      new ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>>();

  private static final ReferenceQueue<Map<String, PropertyFoundResult>> cachedSetterRq =
      new ReferenceQueue<>();


  private static final ReferenceQueue<Map<String, PropertyFoundResult>> cachePropertyRq =
      new ReferenceQueue<>();

  /**
   * static method caching
   */
  public static ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>> cachedMethods =
      new ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>>();

  private static final ReferenceQueue<Map<String, PropertyFoundResult>> cacheMethodRq =
      new ReferenceQueue<>();

  private static String genGetterName(final String prefix, final String name) {
    StringBuilder sb = new StringBuilder(prefix);
    capitalize(sb, name);
    return sb.toString();
  }

  public static enum PropertyType {
    Getter, StaticField, StaticMethod;

    boolean isStaticProperty() {
      return this == StaticField || this == StaticMethod;
    }
  }

  public static Object fastGetProperty(final Object obj, final String name,
      final PropertyType type) {
    final Class<?> clazz = type.isStaticProperty() ? (Class<?>) obj : obj.getClass();
    Map<String, PropertyFoundResult> results = null;

    if (type == PropertyType.StaticMethod) {
      results = getClassPropertyResults(cachedMethods, cacheMethodRq, clazz);
    } else {
      results = getClassPropertyResults(cachedProperties, cachePropertyRq, clazz);
    }

    try {
      PropertyFoundResult result = results.get(name);
      if (result == null) {
        switch (type) {
          case StaticField:
            result = retrieveStaticFieldHandle(results, clazz, name);
            break;
          case Getter:
            result = retrieveGetterHandle(results, clazz, name);
            break;
          case StaticMethod:
            result = retrieveStaticFunction(results, clazz, name);
            break;
        }
      }

      if (type == PropertyType.StaticMethod) {
        return result.func;
      }

      if (result.handle != null) {
        Object ret =
            type == PropertyType.StaticField ? result.handle.invoke() : result.handle.invoke(obj);
        if (result.isBooleanType && !(ret instanceof Boolean)) {
          putDummyHandle(name, results);
          return throwNoSuchPropertyException(
              "Property `" + name + "` not found in java bean: " + obj);
        }
        return ret;
      } else {
        if (type == PropertyType.StaticMethod) {
          return null;
        }
        return throwNoSuchPropertyException(
            "Property `" + name + "` not found in java bean: " + obj);
      }
    } catch (Throwable t) {
      if (!results.containsKey(name)) {
        putDummyHandle(name, results);
      }
      throw sneakyThrow(t);
    }
  }

  public static Object throwNoSuchPropertyException(final String msg) {
    throw new NoSuchPropertyException(msg);
  }

  private static PropertyFoundResult retrieveStaticFieldHandle(
      final Map<String, PropertyFoundResult> results, final Class<?> clazz, final String name)
      throws IllegalAccessException, NoSuchFieldException {
    PropertyFoundResult result;
    Field field = null;
    try {
      field = clazz.getDeclaredField(name);
    } catch (NoSuchFieldException e) {

    }
    if (field != null && Modifier.isStatic(field.getModifiers())) {
      field.setAccessible(true);
      MethodHandle handle = MethodHandles.lookup().unreflectGetter(field);
      result = new PropertyFoundResult(handle, false);
    } else {
      result = new PropertyFoundResult(null, false);
    }
    results.put(name, result);
    return result;
  }

  private static PropertyFoundResult retrieveStaticFunction(
      final Map<String, PropertyFoundResult> results, final Class<?> clazz, final String name)
      throws IllegalAccessException, NoSuchMethodException {
    PropertyFoundResult result;
    List<Method> methods = getStaticMethods(clazz, name);

    if (methods != null && !methods.isEmpty()) {
      // cast the methods into a function.
      ClassMethodFunction func = new ClassMethodFunction(clazz, true, name, name, methods);
      result = new PropertyFoundResult(func);
    } else {
      result = new PropertyFoundResult(null);
    }
    results.put(name, result);
    return result;
  }

  private static PropertyFoundResult retrieveGetterHandle(
      final Map<String, PropertyFoundResult> results, final Class<?> clazz, final String name)
      throws IllegalAccessException {
    PropertyFoundResult result;
    List<Method> methods = getInstanceMethods(clazz, genGetterName("get", name));
    boolean isBooleanType = false;

    if (methods == null || methods.isEmpty()) {
      methods = getInstanceMethods(clazz, genGetterName("is", name));
      isBooleanType = true;
    }

    if ((methods == null || methods.isEmpty()) && name.startsWith("is")) {
      // Fix https://github.com/killme2008/aviatorscript/issues/517
      methods = getInstanceMethods(clazz, name);
      isBooleanType = true;
    }

    if (methods != null && !methods.isEmpty()) {
      Method method = methods.get(0);
      for (Method m : methods) {
        if (m.getParameterTypes().length == 0) {
          method = m;
          break;
        }
      }
      method.setAccessible(true);
      MethodHandle handle = MethodHandles.lookup().unreflect(method);
      result = new PropertyFoundResult(handle, isBooleanType);
    } else {
      result = new PropertyFoundResult(null, isBooleanType);
    }
    results.put(name, result);
    return result;
  }

  private static PropertyFoundResult retrieveSetterHandle(
      final Map<String, PropertyFoundResult> results, final Class<?> clazz, final String name)
      throws IllegalAccessException {
    PropertyFoundResult result;
    List<Method> methods = getInstanceMethods(clazz, genGetterName("set", name));

    if (methods != null && !methods.isEmpty()) {
      Method method = methods.get(0);
      for (Method m : methods) {
        if (m.getParameterTypes().length == 1) {
          method = m;
          break;
        }
      }
      method.setAccessible(true);
      MethodHandle handle = MethodHandles.lookup().unreflect(method);
      result = new PropertyFoundResult(handle, false);
    } else {
      result = new PropertyFoundResult(null, false);
    }
    results.put(name, result);
    return result;
  }

  private static void putDummyHandle(final String name,
      final Map<String, PropertyFoundResult> handles) {
    handles.put(name, new PropertyFoundResult(null, false));
  }

  private static Map<String, PropertyFoundResult> getClassPropertyResults(
      final ConcurrentHashMap<Class<?>, Reference<Map<String, PropertyFoundResult>>> cache,
      final ReferenceQueue<Map<String, PropertyFoundResult>> rq, final Class<?> clazz) {
    Reference<Map<String, PropertyFoundResult>> existsRef = cache.get(clazz);
    Map<String, PropertyFoundResult> results = Collections.emptyMap();
    if (existsRef == null) {
      clearCache(rq, cache);
      results = new ConcurrentHashMap<String, PropertyFoundResult>();
      existsRef = cache.putIfAbsent(clazz,
          new SoftReference<Map<String, PropertyFoundResult>>(results, rq));
    }
    if (existsRef == null) {
      return results;
    }

    results = existsRef.get();
    if (results != null) {
      return results;
    }
    cache.remove(clazz, existsRef);
    return getClassPropertyResults(cache, rq, clazz);
  }


  public static List<Method> getStaticMethods(final Class<?> c, final String methodName) {
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

  static <K, V> void clearCache(final ReferenceQueue<V> rq,
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

  public static class Target {
    Map<String, Object> innerEnv;
    Class<?> innerClazz;
    Object targetObject;

    public Target() {
      super();
    }

    public static Target withObject(final Object target) {
      Target t = new Target();
      t.targetObject = target;
      return t;
    }

    public static Target withEnv(final Map<String, Object> env) {
      Target t = new Target();
      t.innerEnv = env;
      return t;
    }
  }

  static ConcurrentHashMap<MethodKey, Reference<List<Method>>> instanceMethodsCache =
      new ConcurrentHashMap<>();
  static final ReferenceQueue<List<Method>> instanceMethodsRq = new ReferenceQueue<>();


  public static List<Method> getInstanceMethods(final Class<?> clazz, final String methodName) {
    MethodKey key = new MethodKey(clazz, methodName);
    Reference<List<Method>> existingRef = instanceMethodsCache.get(key);
    List<Method> methods = Collections.emptyList();

    if (existingRef == null) {
      clearCache(instanceMethodsRq, instanceMethodsCache);
      methods = getClassInstanceMethods(clazz, methodName);
      existingRef = instanceMethodsCache.putIfAbsent(key,
          new SoftReference<List<Method>>(methods, instanceMethodsRq));
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

  private static List<Method> getClassInstanceMethods(final Class<?> c, final String methodName) {
    List<Method> ret = new ArrayList<Method>();
    for (Method method : c.getMethods()) {
      int modifiers = method.getModifiers();
      if (!Modifier.isStatic(modifiers) && Modifier.isPublic(modifiers)
          && methodName.equals(method.getName())) {
        method.setAccessible(true);
        ret.add(method);
      }
    }
    return ret;
  }

  public static Object invokeStaticMethod(final Class<?> c, final String methodName,
      final List<Method> methods, final Object[] args) {
    return invokeMatchingMethod(methodName, methods, null, args);
  }

  public static Object invokeInstanceMethod(final Class<?> c, final String methodName,
      final Object target, final List<Method> methods, final Object[] args) {
    return invokeMatchingMethod(methodName, methods, target, args);
  }


  public static Object boxArg(final Class<?> paramType, final Object arg) {
    if (!paramType.isPrimitive()) {
      // processing box types
      if (arg instanceof Number) {
        Number n = (Number) arg;
        if (paramType == Integer.class) {
          return n.intValue();
        } else if (paramType == Float.class) {
          return n.floatValue();
        } else if (paramType == Double.class) {
          return n.doubleValue();
        } else if (paramType == Long.class) {
          return n.longValue();
        } else if (paramType == Short.class) {
          return n.shortValue();
        } else if (paramType == Byte.class) {
          return n.byteValue();
        }
      }
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

  public static Object[] boxArgs(final Class<?>[] params, final Object[] args) {
    if (params.length == 0) {
      return null;
    }
    Object[] ret = new Object[params.length];
    for (int i = 0; i < params.length; i++) {
      Object arg = args[i];
      Class<?> paramType = params[i];
      ret[i] = boxArg(paramType, arg);
    }
    return ret;
  }

  private static Set<Class<?>> longClasses = asSet(Long.class, long.class, Integer.class, int.class,
      Byte.class, byte.class, Short.class, short.class, Byte.class, byte.class);

  private static Set<Class<?>> doubleClasses =
      asSet(Double.class, double.class, Float.class, float.class);


  private static Set<Class<?>> asSet(final Class<?>... classes) {
    Set<Class<?>> ret = new HashSet<>();
    for (Class<?> clazz : classes) {
      ret.add(clazz);
    }
    return ret;
  }

  static public boolean paramArgTypeMatch(final Class<?> paramType, final Class<?> argType) {
    if (argType == null) {
      return !paramType.isPrimitive();
    }
    if (paramType == argType || paramType.isAssignableFrom(argType)) {
      return true;
    }

    boolean ret = longClasses.contains(paramType) && longClasses.contains(argType);
    if (ret) {
      return ret;
    }

    ret = doubleClasses.contains(paramType) && doubleClasses.contains(argType);
    if (ret) {
      return ret;
    }

    if (paramType == char.class) {
      return argType == Character.class;
    }
    if (paramType == boolean.class) {
      return argType == Boolean.class;
    }
    return false;
  }

  public static boolean isCongruent(final Class<?>[] params, final Object[] args) {
    boolean ret = false;
    if (args == null) {
      return params.length == 0;
    }
    if (params.length == args.length) {
      ret = true;
      for (int i = 0; ret && i < params.length; i++) {
        Object arg = args[i];
        Class<?> argType = (arg == null) ? null : arg.getClass();
        Class<?> paramType = params[i];
        ret = paramArgTypeMatch(paramType, argType);
      }
    }
    return ret;
  }

  @SuppressWarnings("unchecked")
  public static Object getProperty(final Object target, final String name) {
    final String[] names = Constants.SPLIT_PAT.split(name);
    return fastGetProperty(name, names, null,
        (target instanceof Map) ? Target.withEnv((Map) target) : Target.withObject(target), false,
        0, names.length);
  }

  public static void setProperty(final Map<String, Object> env, String name, final Object val) {
    final String[] names = Constants.SPLIT_PAT.split(name);
    Object target =
        fastGetProperty(name, names, null, Target.withEnv(env), false, 0, names.length - 1);

    if (target == null) {
      // FIXME error msg
      throw new NoSuchPropertyException("Property `" + name + "` not found in java bean:" + env);
    }

    name = names[names.length - 1];
    int arrayIndex = -1;
    String keyIndex = null;

    switch (name.charAt(name.length() - 1)) {
      case ']':
        int idx = name.indexOf("[");
        if (idx < 0) {
          throw new IllegalArgumentException("Should not happen, doesn't contains '['");
        }
        String rawName = name;
        name = name.substring(0, idx);
        arrayIndex = Integer.valueOf(rawName.substring(idx + 1, rawName.length() - 1));
        break;
      case ')':
        idx = name.indexOf("(");
        if (idx < 0) {
          throw new IllegalArgumentException("Should not happen, doesn't contains '('");
        }
        rawName = name;
        name = name.substring(0, idx);
        keyIndex = rawName.substring(idx + 1, rawName.length() - 1);
        break;
    }

    if (arrayIndex >= 0 || keyIndex != null) {
      if (!name.isEmpty()) {
        target = fastGetProperty(target, name, PropertyType.Getter);
      }

      if (arrayIndex >= 0) {
        ArrayUtils.set(target, arrayIndex, boxArg(target.getClass().getComponentType(), val));
      } else {
        ((Map) target).put(keyIndex, val);
      }

    } else if (target instanceof Map) {
      ((Map) target).put(name, val);
    } else {
      final Class<?> clazz = target.getClass();
      Map<String, PropertyFoundResult> results =
          getClassPropertyResults(cachedSettters, cachedSetterRq, clazz);

      try {
        PropertyFoundResult result = results.get(name);
        if (result == null) {
          result = retrieveSetterHandle(results, clazz, name);
        }

        if (result.handle != null) {
          result.handle.invoke(target, boxArg(result.handle.type().parameterType(1), val));
        } else {
          throw new NoSuchPropertyException(
              "Setter not found for property `" + name + "` in class: " + clazz);
        }
      } catch (Throwable t) {
        if (!results.containsKey(name)) {
          putDummyHandle(name, results);
        }
        throw sneakyThrow(t);
      }
    }
  }

  @SuppressWarnings("unchecked")
  public static Object fastGetProperty(final String name, final String[] names,
      final Map<String, Object> env, final Target target, final boolean tryResolveStaticMethod,
      final int offset, final int len) {
    int max = Math.min(offset + len, names.length);
    for (int i = offset; i < max; i++) {
      String rName = AviatorJavaType.reserveName(names[i]);
      rName = rName != null ? rName : names[i];
      int arrayIndex = -1;
      String keyIndex = null;

      // compatible with PropertyUtilsBean indexed and mapped formats.
      // https://commons.apache.org/proper/commons-beanutils/apidocs/org/apache/commons/beanutils/PropertyUtilsBean.html
      switch (rName.charAt(rName.length() - 1)) {
        case ']':
          int idx = rName.indexOf("[");
          if (idx < 0) {
            throw new IllegalArgumentException("Should not happen, doesn't contains '['");
          }
          String rawName = rName;
          rName = rName.substring(0, idx);
          arrayIndex = Integer.valueOf(rawName.substring(idx + 1, rawName.length() - 1));
          break;
        case ')':
          idx = rName.indexOf("(");
          if (idx < 0) {
            throw new IllegalArgumentException("Should not happen, doesn't contains '('");
          }
          rawName = rName;
          rName = rName.substring(0, idx);
          keyIndex = rawName.substring(idx + 1, rawName.length() - 1);
          break;
      }


      Object val = null;

      if (target.innerClazz != null) {
        final AviatorEvaluatorInstance instance = RuntimeUtils.getInstance(env);
        // check innerClazz is allowed
        instance.checkIfClassIsAllowed(true, target.innerClazz);

        if (tryResolveStaticMethod && instance.isFeatureEnabled(Feature.StaticMethods)
            && names.length == 2) {
          val = fastGetProperty(target.innerClazz, rName, PropertyType.StaticMethod);
        } else if (instance.isFeatureEnabled(Feature.StaticFields)) {
          val = fastGetProperty(target.innerClazz, rName, PropertyType.StaticField);
        } else {
          val = fastGetProperty(target.innerClazz, rName, PropertyType.Getter);
        }
      } else {
        // in the format of a.b.[0].c
        if (rName.isEmpty()) {
          if (!(arrayIndex >= 0 || keyIndex != null)) {
            throw new IllegalArgumentException("Invalid format");
          }
          if (target.innerEnv != null) {
            val = target.innerEnv;
          } else {
            val = target.targetObject;
          }
        } else {
          if (target.innerEnv != null) {
            val = target.innerEnv.get(rName);
            if (val == null && i == 0 && env instanceof Env) {
              val = AviatorJavaType.tryResolveAsClass(env, rName);
            }
          } else {
            val = fastGetProperty(target.targetObject, rName, PropertyType.Getter);
          }
        }
      }

      if (arrayIndex >= 0) {
        if (val.getClass().isArray()) {
          val = ArrayUtils.get(val, arrayIndex);
        } else if (val instanceof List) {
          val = ((List) val).get(arrayIndex);
        } else if (val instanceof CharSequence) {
          val = ((CharSequence) val).charAt(arrayIndex);
        } else {
          throw new IllegalArgumentException("Can't access " + val + " with index `" + arrayIndex
              + "`, it's not an array, list or CharSequence");
        }
      }
      if (keyIndex != null) {
        if (Map.class.isAssignableFrom(val.getClass())) {
          val = ((Map) val).get(keyIndex);
        } else {
          throw new IllegalArgumentException(
              "Can't access " + val + " with key `" + keyIndex + "`, it's not a map");
        }
      }

      if (i == max - 1) {
        return val;
      }
      if (val instanceof Map) {
        target.innerEnv = (Map<String, Object>) val;
        target.innerClazz = null;
        target.targetObject = null;
      } else if (val instanceof Class<?>) {
        target.innerClazz = (Class<?>) val;
        target.innerEnv = null;
        target.targetObject = null;
      } else if (val == null) {
        throw new NullPointerException(rName);
      } else {
        target.targetObject = val;
        target.innerEnv = null;
        target.innerClazz = null;
      }
    }
    return throwNoSuchPropertyException("Variable `" + name + "` not found in env: " + env);
  }

  public static Map<String, List<Method>> findMethodsFromClass(final Class<?> clazz,
      final boolean isStatic) {
    Map<String, List<Method>> methodMap = new HashMap<>();

    for (Method method : clazz.getMethods()) {
      int modifiers = method.getModifiers();
      if (Modifier.isPublic(modifiers)) {
        if (isStatic) {
          if (!Modifier.isStatic(modifiers)) {
            continue;
          }
        } else {
          if (Modifier.isStatic(modifiers)) {
            continue;
          }
        }

        if (method.getAnnotation(Ignore.class) != null) {
          continue;
        }

        String methodName = method.getName();
        Function func = method.getAnnotation(Function.class);
        if (func != null) {
          String rename = func.rename();
          if (!rename.isEmpty()) {
            if (!ExpressionParser.isJavaIdentifier(rename)) {
              throw new IllegalArgumentException("Invalid rename `" + rename + "` for method "
                  + method.getName() + " in class " + clazz);
            }
            methodName = func.rename();
          }
        }

        List<Method> methods = methodMap.get(methodName);
        if (methods == null) {
          methods = new ArrayList<>(3);
          methodMap.put(methodName, methods);
        }
        methods.add(method);
      }
    }

    return methodMap;
  }
}
