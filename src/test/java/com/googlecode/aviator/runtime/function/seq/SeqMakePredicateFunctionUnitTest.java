package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.*;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqMakePredicateFunctionUnitTest {

  @Test
  public void testMakePredicate() {
    SeqMakePredicateFunFunction fun = new SeqMakePredicateFunFunction("eq", OperatorType.EQ);

    Map<String, Object> env = new HashMap<String, Object>();
    AviatorJavaType predicateName =
        (AviatorJavaType) fun.call(env, new AviatorRuntimeJavaType("hello"));

    assertNotNull(predicateName);
    AviatorFunction predicate = (AviatorFunction) predicateName.getValue(env);
    assertNotNull(predicate);
    AviatorObject result = predicate.call(null, new AviatorRuntimeJavaType("hello"));
    // equals self
    assertTrue(result.booleanValue(null));

  }


  @Test
  public void testMakePredicate_FixedValue() {
    SeqMakePredicateFunFunction fun =
        new SeqMakePredicateFunFunction("eq", OperatorType.EQ, new AviatorRuntimeJavaType("hello"));
    Map<String, Object> env = new HashMap<String, Object>();
    AviatorJavaType predicateName = (AviatorJavaType) fun.call(env);

    assertNotNull(predicateName);
    AviatorFunction predicate = (AviatorFunction) predicateName.getValue(env);
    assertNotNull(predicate);

    AviatorObject result = predicate.call(null, new AviatorRuntimeJavaType("hello"));
    // equals self
    assertTrue(result.booleanValue(null));

    result = predicate.call(null, new AviatorRuntimeJavaType("he11o"));
    // equals self
    assertFalse(result.booleanValue(null));

  }

}
