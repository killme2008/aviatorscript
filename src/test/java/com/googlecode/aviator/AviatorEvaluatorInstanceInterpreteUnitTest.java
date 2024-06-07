package com.googlecode.aviator;

import org.junit.Before;
import org.junit.Test;

public class AviatorEvaluatorInstanceInterpreteUnitTest extends AviatorEvaluatorInstanceUnitTest {


  @Override
  @Before
  public void setup() {
    super.setup();
    this.instance.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);
  }

  @Test
  public void testTraceEval() throws Exception {
    // ignore
  }
}
