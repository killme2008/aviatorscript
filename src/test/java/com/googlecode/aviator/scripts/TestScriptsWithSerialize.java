package com.googlecode.aviator.scripts;

import org.junit.Before;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.Options;
import com.googlecode.aviator.TestUtils;
import com.googlecode.aviator.runtime.JavaMethodReflectionFunctionMissing;

public class TestScriptsWithSerialize extends TestScripts {

  @Override
  @Before
  public void setup() throws Exception {
    this.instance = AviatorEvaluator.newInstance(EvalMode.ASM);
    this.instance.setOption(Options.SERIALIZABLE, true);
    this.instance.addStaticFunctions("j", TestUtils.class);
    this.instance.setFunctionMissing(JavaMethodReflectionFunctionMissing.getInstance());
    this.testSerialize = true;
  }

}
