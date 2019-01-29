package com.googlecode.aviator.runtime.function.seq;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * seq.list function to new an array list.
 *
 * @since 4.1.2
 * @author dennis
 *
 */
public class SeqNewListFunction extends AbstractVariadicFunction {

  @Override
  public String getName() {
    return "seq.list";
  }

  @Override
  public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {
    List<Object> list = new ArrayList<>();

    for (AviatorObject obj : args) {
      list.add(obj.getValue(env));
    }

    return new AviatorRuntimeJavaType(list);
  }


}
