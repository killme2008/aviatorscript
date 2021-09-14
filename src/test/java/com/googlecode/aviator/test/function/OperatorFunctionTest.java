package com.googlecode.aviator.test.function;

import static com.googlecode.aviator.TestUtils.assertEquals;
import static org.junit.Assert.fail;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;
import org.junit.After;
import org.junit.Before;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.EvalMode;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;

public class OperatorFunctionTest {

  protected AviatorEvaluatorInstance instance;

  @Before
  public void setup() {
    this.instance = AviatorEvaluator.newInstance(EvalMode.ASM);
  }

  @After
  public void tearDown() {
    this.instance.getOpsMap().clear();
  }

  @Test
  public void testCustomArrayIndexOperator() {
    Map<String, Object> env = new HashMap<>();
    env.put("a", Arrays.asList(4, 5, 6));
    assertEquals(4, this.instance.execute("a[0]", env));
    try {
      assertEquals(6, this.instance.execute("a[3]", env));
      fail();
    } catch (Exception e) {

    }
    this.instance.addOpFunction(OperatorType.INDEX, new AbstractFunction() {

      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        return arg1.getElement(env, arg2.sub(AviatorLong.valueOf(1), env));
      }

      @Override
      public String getName() {
        return "[]";
      }
    });

    assertEquals(6, this.instance.execute("a[3]", env));
    assertEquals(5, this.instance.execute("a[2]", env));
    assertEquals(4, this.instance.execute("a[1]", env));
    try {
      assertEquals(4, this.instance.execute("a[0]", env));
      fail();
    } catch (Exception e) {

    }
  }

  @Test
  public void testCustomUnaryOperatorFunction() {
    try {
      assertEquals("3", this.instance.exec("!3"));
      fail();
    } catch (Exception e) {
    }
    this.instance.addOpFunction(OperatorType.NOT, new AbstractFunction() {

      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
        return new AviatorString(arg1.getValue(env).toString());
      }

      @Override
      public String getName() {
        return "!";
      }
    });

    assertEquals("3", this.instance.exec("!3"));
    assertEquals("4", this.instance.exec("!a", 4));
    assertEquals("3.2", this.instance.exec("!3.2"));
  }

  @Test
  public void testCustomBinOperatorFunction() {
    try {
      assertEquals("hello world", this.instance.execute("'hello' & ' world'"));
      fail();
    } catch (Exception e) {
    }
    this.instance.addOpFunction(OperatorType.BIT_AND, new AbstractFunction() {

      @Override
      public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
          final AviatorObject arg2) {
        return new AviatorString(arg1.getValue(env).toString() + arg2.getValue(env).toString());
      }

      @Override
      public String getName() {
        return "&";
      }
    });

    assertEquals("43", this.instance.exec("a&3", 4));
    assertEquals("43", this.instance.exec("4&3", 4));
    assertEquals("hello world", this.instance.execute("'hello' & ' world'"));
    assertEquals("hello world", this.instance.exec("a&' world'", "hello"));
    assertEquals("hello3 world", this.instance.exec("a & 3 & ' world'", "hello"));
    Map<String, Object> env = new HashMap<>();
    env.put("list", Arrays.asList(1, 2, 3));
    assertEquals("123", this.instance.execute("reduce(list, &, '')", env));
  }
}
