package com.googlecode.aviator.annotation;

import java.lang.annotation.ElementType;
import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.Target;

/**
 * Ignore annotation for method to ignore importing the java class methods.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.4
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Ignore {

}
