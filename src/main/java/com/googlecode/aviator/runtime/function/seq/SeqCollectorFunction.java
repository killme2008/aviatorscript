package com.googlecode.aviator.runtime.function.seq;

import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.Sequence;

/**
 * seq.collector(seq) to create a collector for this sequence.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class SeqCollectorFunction extends AbstractFunction {

  private static final long serialVersionUID = -3174913891253579826L;

  private SeqCollectorFunction() {}

  public static final SeqCollectorFunction INSTANCE = new SeqCollectorFunction();

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    final Sequence<?> seq = RuntimeUtils.seq(arg1.getValue(env), env);

    return AviatorRuntimeJavaType.valueOf(seq.newCollector(0));
  }

  @Override
  public String getName() {
    return "seq.collector";
  }

}
