package com.googlecode.aviator.scripts;

import org.junit.Before;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.Options;

public class TestScriptsWithIntepreter extends TestScripts {

  @Override
  @Before
  public void setup() throws Exception {
    super.setup();
    this.instance.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);
  }

}
