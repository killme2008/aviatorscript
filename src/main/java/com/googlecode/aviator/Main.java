package com.googlecode.aviator;

import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;

/**
 * AviatorScript bootstrap
 *
 * @author dernnis(killme2008@gmail.com)
 *
 */
public class Main {

  public static void main(final String[] args) throws Exception {
    if (args == null || args.length < 1) {
      System.err.println("Useage: java com.googlecode.aviator.Main [file] [args]");
      System.exit(1);
    }
    String path = args[0];

    String[] remainArgs = new String[args.length - 1];
    System.arraycopy(args, 1, remainArgs, 0, remainArgs.length);

    AviatorEvaluator.getInstance().setOption(Options.FEATURE_SET, Feature.getFullFeatures());
    AviatorEvaluator.getInstance()
        .setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());

    Expression exp = AviatorEvaluator.getInstance().compileScript(path);

    exp.execute(exp.newEnv("ARGS", remainArgs));

  }
}
