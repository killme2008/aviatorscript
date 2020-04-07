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


  private static final long serialVersionUID = -2581715177871593829L;

  @Override
  public String getName() {
    return "seq.map";
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    if (args.length % 2 != 0) {
      throw new IllegalArgumentException("Expect arguments in even number as key/value pairs.");
    }

    Map<Object, Object> map = new HashMap<>(args != null ? args.length / 2 : 10);
    for (int i = 0; i < args.length;) {
      map.put(args[i].getValue(env), args[i + 1].getValue(env));
      i += 2;
    }

    return AviatorRuntimeJavaType.valueOf(map);
  }

}
