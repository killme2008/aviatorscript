package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.Feature;
import com.googlecode.aviator.Options;

/**
 * Configure engine example
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ConfigureInstanceExample {

  public static void main(final String[] args) {
    AviatorEvaluatorInstance instance = AviatorEvaluator.newInstance();

    instance.setOption(Options.USE_USER_ENV_AS_TOP_ENV_DIRECTLY, false);
    instance.setOption(Options.FEATURE_SET, Feature.asSet(Feature.Assignment, Feature.ForLoop,
        Feature.WhileLoop, Feature.Lambda, Feature.Let));

    System.out.println(instance
        .execute("let square = lambda(x) -> x*2 end; for x in range(0, 10) { p(square(x)); }"));
  }
}
