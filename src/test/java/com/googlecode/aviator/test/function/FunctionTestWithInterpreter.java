package com.googlecode.aviator.test.function;

import org.junit.Before;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.EvalMode;

public class FunctionTestWithInterpreter extends FunctionTest {

  @Override
  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance(EvalMode.INTERPRETER);
  }
}
