package com.googlecode.aviator;

import java.util.Map;

/**
 * Processing env before or after executing expression.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public interface EnvProcessor {
  /**
   * This method will be called before executing the expression.
   *
   * @param env the env object
   * @param script the script object
   */
  void beforeExecute(Map<String, Object> env, Expression script);

  /**
   * This method will be called after executing the expression.
   *
   * @param env the env object
   * @param script the script object
   */
  void afterExecute(Map<String, Object> env, Expression script);
}
