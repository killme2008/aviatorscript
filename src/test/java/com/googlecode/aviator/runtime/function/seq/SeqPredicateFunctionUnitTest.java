package com.googlecode.aviator.runtime.function.seq;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorString;
import org.junit.Test;
import java.util.HashMap;
import java.util.Map;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;


public class SeqPredicateFunctionUnitTest {

  @Test
  public void testPredicate_eq() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("eq", OperatorType.EQ, AviatorRuntimeJavaType.valueOf("hello"));

    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("he1lo"));
    assertFalse(result.booleanValue(null));
  }


  @Test
  public void testPredicate_neq() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("neq", OperatorType.NEQ, AviatorRuntimeJavaType.valueOf("hello"));

    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    assertFalse(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("he1lo"));
    assertTrue(result.booleanValue(null));
  }


  @Test
  public void testPredicate_gt() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("gt", OperatorType.GT, AviatorRuntimeJavaType.valueOf("hello"));

    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    assertFalse(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("iello"));
    assertTrue(result.booleanValue(null));
  }


  @Test
  public void testPredicate_ge() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("ge", OperatorType.GE, AviatorRuntimeJavaType.valueOf("hello"));

    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("iello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("aello"));
    assertFalse(result.booleanValue(null));
  }


  @Test
  public void testPredicate_lt() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("lt", OperatorType.LT, AviatorRuntimeJavaType.valueOf("hello"));

    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    assertFalse(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("ae1lo"));
    assertTrue(result.booleanValue(null));
  }


  @Test
  public void testPredicate_le() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("le", OperatorType.LE, AviatorRuntimeJavaType.valueOf("hello"));

    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("ae1lo"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, AviatorRuntimeJavaType.valueOf("ie1lo"));
    assertFalse(result.booleanValue(null));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testPredicate_IllegalArguments() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("le", OperatorType.LE, AviatorRuntimeJavaType.valueOf("hello"));
    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"), null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testPredicate_IllegalOperator() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("and", OperatorType.AND, AviatorRuntimeJavaType.valueOf("hello"));
    AviatorObject result = fun.call(null, AviatorRuntimeJavaType.valueOf("hello"));
  }

  @Test
  public void testPredicate_property() {
    Map<String, String> data = new HashMap<>();
    for (int i = 0; i < 5; i++) {
      data.put("key" + i, "value" + i);
    }
    SeqPredicateFunction predicate = new SeqPredicateFunction("eq_temp_1", OperatorType.EQ,
        new AviatorString("value1"), new AviatorString("key1"));
    AviatorObject result = predicate.call(null, AviatorRuntimeJavaType.valueOf(data));
    assertTrue(result.booleanValue(null));
  }
}
