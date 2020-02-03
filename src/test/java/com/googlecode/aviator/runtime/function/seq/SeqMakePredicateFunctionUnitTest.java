package com.googlecode.aviator.runtime.function.seq;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import java.util.HashMap;
import java.util.Map;
import org.junit.Test;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


public class SeqMakePredicateFunctionUnitTest {

  @Test
  public void testMakePredicate() {
    SeqMakePredicateFunFunction fun = new SeqMakePredicateFunFunction("eq", OperatorType.EQ);

    Map<String, Object> env = new HashMap<String, Object>();
    AviatorObject predicateName = fun.call(env, AviatorRuntimeJavaType.valueOf("hello"));

    assertNotNull(predicateName);
    AviatorFunction predicate = (AviatorFunction) predicateName.getValue(env);
    assertNotNull(predicate);
    AviatorObject result = predicate.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    // equals self
    assertTrue(result.booleanValue(null));

  }


  @Test
  public void testMakePredicate_FixedValue() {
    SeqMakePredicateFunFunction fun = new SeqMakePredicateFunFunction("eq", OperatorType.EQ,
        AviatorRuntimeJavaType.valueOf("hello"));
    Map<String, Object> env = new HashMap<String, Object>();
    AviatorObject predicateName = fun.call(env);

    assertNotNull(predicateName);
    AviatorFunction predicate = (AviatorFunction) predicateName.getValue(env);
    assertNotNull(predicate);

    AviatorObject result = predicate.call(null, AviatorRuntimeJavaType.valueOf("hello"));
    // equals self
    assertTrue(result.booleanValue(null));

    result = predicate.call(null, AviatorRuntimeJavaType.valueOf("he11o"));
    // equals self
    assertFalse(result.booleanValue(null));

  }

}
