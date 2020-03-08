package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;

/**
 * Run a script under examples folder.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class RunScriptExample {

  public static void main(final String[] args) throws Exception {
    // Enable require and load functions.
    AviatorEvaluator.getInstance().setOption(Options.ENABLE_REQUIRE_LOAD_SCRIPTS, true);
    // You can trry to test every script in examples folder by changing the file name.
    Expression exp = AviatorEvaluator.getInstance().compileScript("examples/test_qsort.av");

    exp.execute();

  }

}
