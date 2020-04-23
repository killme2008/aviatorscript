package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;

/**
 * Run a script under examples folder.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class RunScriptExample {

  public static void main(final String[] args) throws Exception {
    // Enable java method invocation by reflection.
    AviatorEvaluator.getInstance()
    .setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
    // You can trry to test every script in examples folder by changing the file name.
    Expression exp = AviatorEvaluator.getInstance().compileScript("examples/hello.av");

    exp.execute();

  }

}
