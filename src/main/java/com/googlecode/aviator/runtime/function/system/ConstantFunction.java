package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Constant function to return the argument itself.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 4.2.5
 *
 */
public class ConstantFunction extends AbstractVariadicFunction {

  private final String name;
  private final AviatorObject result;



  public ConstantFunction(final String name, final AviatorObject result) {
    super();
    this.name = name;
    this.result = result;
  }

  @Override
  public String getName() {
    return this.name;
  }

  @Override
  public AviatorObject variadicCall(final Map<String, Object> env, final AviatorObject... args) {
    return this.result;
  }

}
