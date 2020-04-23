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


  private static final long serialVersionUID = -8247803628833006273L;

  @Override
  public String getName() {
    return "seq.set";
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    Set<Object> set = new HashSet<>(args != null ? args.length : 10);

    if (args != null) {
      for (AviatorObject obj : args) {
        set.add(obj.getValue(env));
      }
    }

    return AviatorRuntimeJavaType.valueOf(set);
  }


}
