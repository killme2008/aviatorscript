package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * seq(obj) function to cast a object into sequence, throw an runtime exception if fail.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class SeqFunction extends AbstractFunction {

  private static final long serialVersionUID = 1796744329506734585L;


  public static final SeqFunction INSTANCE = new SeqFunction();

  @Override
  public String getName() {
    return "seq";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return AviatorRuntimeJavaType.valueOf(RuntimeUtils.seq(arg1.getValue(env), env));
  }

}
