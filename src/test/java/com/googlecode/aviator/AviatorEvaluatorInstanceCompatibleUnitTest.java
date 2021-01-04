package com.googlecode.aviator;

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.fail;
import java.math.MathContext;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.exception.UnsupportedFeatureException;

public class AviatorEvaluatorInstanceCompatibleUnitTest extends AviatorEvaluatorInstanceUnitTest {


  @Override
  @Before
  public void setup() {
    super.setup();
    this.instance.setOption(Options.FEATURE_SET, Feature.getCompatibleFeatures());
  }

  @Override
  @Test(expected = UnsupportedFeatureException.class)
  public void testMaxLoopCount() {
    super.testMaxLoopCount();
  }

  @Override
  @Test
  public void testClassAllowList() {
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
