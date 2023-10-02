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
  }

  @Test
  public void testInterpreteEngine() throws Exception {
    this.engine.setOption(Options.EVAL_MODE, EvalMode.INTERPRETER);

    this.engine.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
    testLiteral(engine);
    testClassExpression(engine);

    this.engine.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.EVAL);
    testLiteral(engine);
    testClassExpression(engine);

  }

  @Test
  public void testAsmEngine() throws Exception {
    this.engine.setOption(Options.EVAL_MODE, EvalMode.ASM);

    this.engine.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.COMPILE);
    testLiteral(engine);
    testClassExpression(engine);

    this.engine.setOption(Options.OPTIMIZE_LEVEL, AviatorEvaluator.EVAL);
    testLiteral(engine);
    testClassExpression(engine);

  }

  public static void testLiteral(AviatorEvaluatorInstance engine) throws Exception {
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

  public static void testClassExpression(AviatorEvaluatorInstance engine) throws Exception {
    Expression exp = engine.compile("p(a);if(a>b) { a } else { b }", true);
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
