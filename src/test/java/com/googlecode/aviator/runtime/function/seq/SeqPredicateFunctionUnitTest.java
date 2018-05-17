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
        new SeqPredicateFunction("eq", OperatorType.EQ, new AviatorRuntimeJavaType("hello"));

    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("he1lo"));
    assertFalse(result.booleanValue(null));
  }


  @Test
  public void testPredicate_neq() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("neq", OperatorType.NEQ, new AviatorRuntimeJavaType("hello"));

    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    assertFalse(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("he1lo"));
    assertTrue(result.booleanValue(null));
  }


  @Test
  public void testPredicate_gt() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("gt", OperatorType.GT, new AviatorRuntimeJavaType("hello"));

    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    assertFalse(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("iello"));
    assertTrue(result.booleanValue(null));
  }


  @Test
  public void testPredicate_ge() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("ge", OperatorType.GE, new AviatorRuntimeJavaType("hello"));

    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("iello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("aello"));
    assertFalse(result.booleanValue(null));
  }


  @Test
  public void testPredicate_lt() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("lt", OperatorType.LT, new AviatorRuntimeJavaType("hello"));

    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    assertFalse(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("ae1lo"));
    assertTrue(result.booleanValue(null));
  }


  @Test
  public void testPredicate_le() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("le", OperatorType.LE, new AviatorRuntimeJavaType("hello"));

    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("ae1lo"));
    assertTrue(result.booleanValue(null));

    result = fun.call(null, new AviatorRuntimeJavaType("ie1lo"));
    assertFalse(result.booleanValue(null));

  }


  @Test(expected = IllegalArgumentException.class)
  public void testPredicate_IllegalArguments() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("le", OperatorType.LE, new AviatorRuntimeJavaType("hello"));
    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"), null);

  }


  @Test(expected = ExpressionRuntimeException.class)
  public void testPredicate_IllegalOperator() {
    SeqPredicateFunction fun =
        new SeqPredicateFunction("and", OperatorType.AND, new AviatorRuntimeJavaType("hello"));
    AviatorObject result = fun.call(null, new AviatorRuntimeJavaType("hello"));
  }

  @Test
  public void testPredicate_property() {
    Map<String, String> data = new HashMap<>();
    for (int i = 0; i < 5; i++) {
      data.put("key" + i, "value" + i);
    }
    SeqPredicateFunction predicate = new SeqPredicateFunction("eq_temp_1", OperatorType.EQ,
        new AviatorString("value1"), new AviatorString("key1"));
    AviatorObject result = predicate.call(null, new AviatorRuntimeJavaType(data));
    assertTrue(result.booleanValue(null));
  }
}
