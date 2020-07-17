package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * into(to_coll, from_coll) Adds all elements in from_coll into to_coll by seq.add(to_coll, element)
 * and return the to_coll.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class SeqIntoFunction extends AbstractFunction {

  private static final long serialVersionUID = 1426576856324636917L;
  private static final AviatorFunction SEQ_ADD = new SeqAddFunction();

  @Override
  public String getName() {
    return "into";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    Object fromSeq = arg2.getValue(env);
    AviatorObject result = arg1;

    for (Object e : RuntimeUtils.seq(fromSeq, env)) {
      result = SEQ_ADD.call(env, result, AviatorRuntimeJavaType.valueOf(e));
    }

    return result;
  }

}
