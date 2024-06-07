package com.googlecode.aviator;

import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.TimeoutException;

public class AviatorEvaluatorInstanceInterpreteUnitTest extends AviatorEvaluatorInstanceUnitTest {


  @Override
  @Before
  public void setup() {
    super.setup();
    this.instance.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);
    this.instance.setOption(Options.EVAL_TIMEOUT_MS, 50);
  }

  @Test
  public void testTraceEval() throws Exception {
    // ignore
  }

  @Test(expected = TimeoutException.class)
  public void testEvalTimeout() {
    this.instance.execute("while(true) { }");
  }

}
