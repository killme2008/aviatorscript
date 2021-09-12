package com.googlecode.aviator.scripts;

import org.junit.Before;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;

public class TestScriptsWithIntepreter extends TestScripts {

  @Override
  @Before
  public void setup() throws Exception {
    this.instance = AviatorEvaluator.newInstance(EvalMode.INTERPRETER);
    this.instance.addStaticFunctions("j", TestUtils.class);
    this.instance.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
  }

}
