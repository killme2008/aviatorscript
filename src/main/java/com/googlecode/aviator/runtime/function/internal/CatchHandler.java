package com.googlecode.aviator.runtime.function.internal;

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
  private final Class<?> exceptionClass;

  public CatchHandler(final AviatorFunction func, String exceptionClass) {
    super();
    this.func = func;
    try {
      if (!exceptionClass.contains(".")) {
        exceptionClass = "java.lang." + exceptionClass;
      }
      this.exceptionClass = Class.forName(exceptionClass);
    } catch (Exception e) {
      throw Reflector.sneakyThrow(e);
    }
  }

  public AviatorFunction getFunc() {
    return this.func;
  }

  public Class<?> getExceptionClass() {
    return this.exceptionClass;
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
