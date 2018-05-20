package com.googlecode.aviator;

import com.googlecode.aviator.utils.Env;

/**
 * Test helper
 *
 * @author dennis
 *
 */
public class TestUtils {

  public static Env getTestEnv() {
    Env env = new Env();
    env.setInstance(AviatorEvaluator.getInstance());
    return env;
  }
}
