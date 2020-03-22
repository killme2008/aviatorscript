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
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    List<Object> list = new ArrayList<>(args != null ? args.length : 10);

    for (AviatorObject obj : args) {
      list.add(obj.getValue(env));
    }

    return AviatorRuntimeJavaType.valueOf(list);
  }


}
