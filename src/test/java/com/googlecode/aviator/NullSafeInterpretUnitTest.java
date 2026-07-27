package com.googlecode.aviator;

import org.junit.Before;

public class NullSafeInterpretUnitTest extends NullSafeUnitTest {

  @Override
  @Before
  public void setup() {
    super.setup();
    this.instance.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);
  }
}
