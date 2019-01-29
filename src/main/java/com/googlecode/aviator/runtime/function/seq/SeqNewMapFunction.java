package com.googlecode.aviator.runtime.function.seq;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * seq.map function to new a hash map.
 *
 * @since 4.1.2
 * @author dennis
 *
 */
public class SeqNewMapFunction extends AbstractVariadicFunction {

  @Override
  public String getName() {
    return "seq.map";
  }

  @Override
  public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {
    Map<Object, Object> map = new HashMap<>();

    if (args.length % 2 != 0) {
      throw new IllegalArgumentException("Expect arguments in even number as key/value pairs.");
    }

    for (int i = 0; i < args.length;) {
      map.put(args[i].getValue(env), args[i + 1].getValue(env));
      i += 2;
    }

    return new AviatorRuntimeJavaType(map);
  }

}
