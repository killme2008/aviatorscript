package com.googlecode.aviator.runtime.function.internal;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Reflector;

/**
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class CatchHandler extends AviatorObject {
  /**
   *
   */
  private static final long serialVersionUID = 2718902412145274738L;
  private final AviatorFunction func;
  private final List<Class<?>> exceptionClasses;

  public CatchHandler(final AviatorFunction func, final List<String> exceptionClassNames) {
    super();
    this.func = func;
    this.exceptionClasses = new ArrayList<>(exceptionClassNames.size());
    for (String exceptionClass : exceptionClassNames) {
      try {
        if (!exceptionClass.contains(".")) {
          exceptionClass = "java.lang." + exceptionClass;
        }
        this.exceptionClasses.add(Class.forName(exceptionClass));
      } catch (Exception e) {
        throw Reflector.sneakyThrow(e);
      }
    }
  }

  public AviatorFunction getFunc() {
    return this.func;
  }

  public boolean isMatch(final Class<?> eClass) {
    for (Class<?> clazz : this.exceptionClasses) {
      if (clazz.isAssignableFrom(eClass)) {
        return true;
      }
    }
    return false;
  }

  @Override
  public int innerCompare(final AviatorObject other, final Map<String, Object> env) {
    throw new UnsupportedOperationException();
  }

  @Override
  public AviatorType getAviatorType() {
    return AviatorType.JavaType;
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    return this;
  }

}
