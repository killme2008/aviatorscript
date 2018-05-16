package com.googlecode.aviator.runtime.function;

import static org.junit.Assert.*;
import org.junit.Test;
import com.googlecode.aviator.AviatorEvaluator;
import com.googlecode.aviator.AviatorEvaluatorInstance;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.seq.SeqMapFunction;
import com.googlecode.aviator.runtime.function.system.BinaryFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;


public class FunctionUtilsUnitTest {

  @Test
  public void testGetFunction_Normal() {
    AviatorFunction fun =
        FunctionUtils.getFunction(new AviatorJavaType("map"), AviatorEvaluator.FUNC_MAP, 2);
    assertNotNull(fun);
    assertTrue(fun instanceof SeqMapFunction);
  }


  @Test
  public void testGetFunction_sub() {
    AviatorFunction fun =
        FunctionUtils.getFunction(new AviatorJavaType("-"), AviatorEvaluator.FUNC_MAP, 2);
    assertNotNull(fun);
    assertTrue(fun instanceof BinaryFunction);
    assertEquals("-sub", fun.getName());
    assertEquals(OperatorType.SUB, ((BinaryFunction) fun).getOpType());

  }


  @Test
  public void testGetFunction_neg() {
    AviatorFunction fun =
        FunctionUtils.getFunction(new AviatorJavaType("-"), AviatorEvaluator.FUNC_MAP, 1);
    assertNotNull(fun);
    assertTrue(fun instanceof BinaryFunction);
    assertEquals("-neg", fun.getName());
    assertEquals(OperatorType.NEG, ((BinaryFunction) fun).getOpType());

  }
}
