package com.googlecode.aviator.example;

import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.Expression;
import com.googlecode.aviator.Options;

/**
 * Interpreter example
 *
 * @since 5.3
 * @author dennis(killme2008@gmail.com)
 *
 */
public class InterpreterExample {

  public static void main(final String[] args) {
    AviatorEvaluatorInstance engine = AviatorEvaluator.newInstance(EvalMode.INTERPRETER);
    // Enable tracing eval procedure(don't open it in production)
    engine.setOption(Options.TRACE_EVAL, true);

    Expression exp = engine.compile("score > 80 ? 'good' : 'bad'");
    System.out.println(exp.execute(exp.newEnv("score", 100)));
    System.out.println(exp.execute(exp.newEnv("score", 50)));
  }
}
