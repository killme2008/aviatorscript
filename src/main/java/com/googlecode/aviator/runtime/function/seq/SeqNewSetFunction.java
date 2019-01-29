package com.googlecode.aviator.runtime.function.seq;

import java.util.HashSet;
import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * seq.set function to new a hash set.
 *
 * @since 4.1.2
 * @author dennis
 *
 */
public class SeqNewSetFunction extends AbstractVariadicFunction {

  @Override
  public String getName() {
    return "seq.set";
  }

  @Override
  public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {
    Set<Object> set = new HashSet<>();

    for (AviatorObject obj : args) {
      set.add(obj.getValue(env));
    }

    return new AviatorRuntimeJavaType(set);
  }


}
