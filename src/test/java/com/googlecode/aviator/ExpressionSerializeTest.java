package com.googlecode.aviator;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertNotSame;
import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import org.junit.Before;
import org.junit.Test;

public class ExpressionSerializeTest {
  private AviatorEvaluatorInstance engine;

  @Before
  public void setup() {
    this.engine = AviatorEvaluator.newInstance();
    this.engine.setOption(Options.SERIALIZABLE, true);
    this.engine.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
    this.engine.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);
  }

  @Test
  public void testLiteral() throws Exception {
    Expression exp = engine.compile("1+2");
    assertEquals((long) exp.execute(), 3L);
    byte[] bs = null;
    try (ByteArrayOutputStream out = new ByteArrayOutputStream()) {
      ObjectOutputStream output = engine.newObjectOutputStream(out);
      output.writeObject(exp);
      output.close();
      bs = out.toByteArray();
    }

    assertNotNull(bs);

    try (ByteArrayInputStream in = new ByteArrayInputStream(bs)) {
      ObjectInputStream input = engine.newObjectInputStream(in);
      Expression deExp = (Expression) input.readObject();
      assertNotSame(deExp, exp);
      assertEquals((long) deExp.execute(), 3L);
    }
  }

  @Test
  public void testClassExpression() throws Exception {
    Expression exp = engine.compile("print(a);if(a>b) { a } else { b }", true);
    assertEquals((long) exp.execute(exp.newEnv("a", 3L, "b", 2L)), 3L);
    byte[] bs = null;
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    try (ObjectOutputStream output = engine.newObjectOutputStream(out)) {
      output.writeObject(exp);
    }
    out.close();
    bs = out.toByteArray();

    assertNotNull(bs);

    try (ByteArrayInputStream in = new ByteArrayInputStream(bs)) {
      ObjectInputStream input = engine.newObjectInputStream(in);
      Expression deExp = (Expression) input.readObject();
      assertNotSame(deExp, exp);
      assertEquals((long) deExp.execute(deExp.newEnv("a", 3L, "b", 2L)), 3L);
    }
  }
}
