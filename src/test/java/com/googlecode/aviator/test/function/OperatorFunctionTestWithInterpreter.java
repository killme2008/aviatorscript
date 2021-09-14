package com.googlecode.aviator.test.function;

import org.junit.After;
import org.junit.Before;
import org.junit.runner.notification.RunListener.ThreadSafe;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.EvalMode;

@ThreadSafe
public class OperatorFunctionTestWithInterpreter extends OperatorFunctionTest {
  @Override
  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance(EvalMode.INTERPRETER);
  }

  @Override
  @After
  public void tearDown() {
    this.instance.getOpsMap().clear();
  }
}
