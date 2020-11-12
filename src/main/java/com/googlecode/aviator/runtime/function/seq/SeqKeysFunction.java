package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * seq.keys(map) to retrieve keys sequence of the map.
 *
 * @since 5.2.0
 * @author dennis
 *
 */
public class SeqKeysFunction extends AbstractFunction {


  private static final long serialVersionUID = -8707187642296260032L;

  private SeqKeysFunction() {

  }

  public static final SeqKeysFunction INSTANCE = new SeqKeysFunction();

  @Override
  public String getName() {
    return "seq.keys";
  }

  @SuppressWarnings("rawtypes")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {

    Map m = (Map) arg1.getValue(env);

    if (m == null) {
      return AviatorNil.NIL;
    }

    return AviatorRuntimeJavaType.valueOf(m.keySet());
  }

}
