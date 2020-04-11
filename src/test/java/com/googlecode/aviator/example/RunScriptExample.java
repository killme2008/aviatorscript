package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;

/**
 * Run a script under examples folder.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class RunScriptExample {

  public static void main(final String[] args) throws Exception {
    // You can trry to test every script in examples folder by changing the file name.
    Expression exp = AviatorEvaluator.getInstance().compileScript("examples/test_qsort.av");

    exp.execute();

  }

}
