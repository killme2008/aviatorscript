package com.googlecode.aviator.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Function annotation for method to import java class public method.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.4
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Function {
  /**
   * Rename the method name as the imported function name.
   *
   * @return the new name for function.
   */
  String rename() default "";
}
