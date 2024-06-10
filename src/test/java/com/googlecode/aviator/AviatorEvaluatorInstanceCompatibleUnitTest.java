package com.googlecode.aviator;

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.fail;
import java.math.MathContext;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.exception.UnsupportedFeatureException;

public class AviatorEvaluatorInstanceCompatibleUnitTest extends AviatorEvaluatorInstanceUnitTest {


  @Override
  @Before
  public void setup() {
    super.setup();
    this.instance.setOption(Options.FEATURE_SET, Feature.getCompatibleFeatures());
  }

  @Test
  public void testIssue549() {
    // ignore
  }

  @Test
  public void testSandboxMode() {
    this.instance.enableSandboxMode();
    try {
      this.instance.execute("new java.util.Date()");
    } catch (UnsupportedFeatureException e) {
      // ignore
    }

    try {
      this.instance.execute("Math.abs(-1)");
    } catch (FunctionNotFoundException e) {
      // ignore
    }

    try {
      this.instance.execute("System.exit(1)");
    } catch (FunctionNotFoundException e) {
      // ignore
    }

    try {
      this.instance.execute("Math.PI");
    } catch (ExpressionRuntimeException e) {
      // ignore
    }
    try {
      assertNull(this.instance.execute("__env__"));
    } catch (UnsupportedFeatureException e) {

    }
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testMaxLoopCount() {
    super.testMaxLoopCount();
  }

  @Test
  public void testEvalTimeout() {
    // ignore
  }

  @Override
  @Test
  public void testIssue476() {
    // ignore
  }

  @Test
  public void testEvalTimeoutAndTryAgain() throws Exception {
    // ignore
  }

  @Override
  @Test
  public void testClassAllowList() {
    // ignore
  }

  @Override
  public void testAssignableClazzWhiteList() {
    // ignore
  }

  @Override
  @Test
  public void testDefaultOptionValues() {
    assertEquals(this.instance.getOption(Options.TRACE_EVAL), false);
    assertEquals(this.instance.getOption(Options.FEATURE_SET), Feature.getCompatibleFeatures());
    assertEquals(this.instance.getOption(Options.ALWAYS_PARSE_FLOATING_POINT_NUMBER_INTO_DECIMAL),
        false);
    assertEquals(this.instance.getOption(Options.OPTIMIZE_LEVEL), AviatorEvaluator.EVAL);
    assertEquals(this.instance.getOption(Options.MATH_CONTEXT), MathContext.DECIMAL128);
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testIf() {
    this.instance.execute("if(true) { 3 } else { 4} ");
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testReturn() {
    this.instance.execute("return 'hello';");
  }


  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testFor() {
    this.instance.execute("sum = 0; for x in range(0, 10) { sum = sum + x ;} return sum;");
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testWhile() {
    this.instance
        .execute("sum = 0; x = 0; while(x < 10) { sum = sum + x ; x = x + 1;} return sum;");
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testLet() {
    this.instance.execute("let x =1 ; { let x = 2 ; } return x;");
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testFn() {
    this.instance.execute("fn square(x) { x * 2 }  square(3)");
  }

  @Test
  public void testInstanceVar() {
    assertEquals(this.instance, this.instance.execute("__instance__"));
    this.instance.disableFeature(Feature.InternalVars);
    try {
      this.instance.execute("__instance__");
      fail();
    } catch (UnsupportedFeatureException e) {

    }
    this.instance.enableFeature(Feature.InternalVars);
    assertEquals(this.instance, this.instance.execute("__instance__"));
  }
}
