package com.googlecode.aviator.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Import annotation for class to special importing methods scope.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.4
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.TYPE)
public @interface Import {
  /**
   * Setting import method scope, default is {ImportScope.Instance, ImportScope.Static}
   *
   * @return the import method scopes array.
   */
  ImportScope[] scopes() default {ImportScope.Instance, ImportScope.Static};

  /**
   * Setting the namespace.
   *
   * @return the namespace for imported methods.
   */
  String ns() default "";
}
