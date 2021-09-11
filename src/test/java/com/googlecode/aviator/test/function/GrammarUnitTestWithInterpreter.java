package com.googlecode.aviator.test.function;

import org.junit.AfterClass;
import org.junit.BeforeClass;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.Options;

public class GrammarUnitTestWithInterpreter extends GrammarUnitTest {

  @BeforeClass
  public static void setup() {
    AviatorEvaluator.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);
  }

  @AfterClass
  public static void tearDown() {
    AviatorEvaluator.setOption(Options.EVAL_MODE, EvalMode.ASM);
  }
}
