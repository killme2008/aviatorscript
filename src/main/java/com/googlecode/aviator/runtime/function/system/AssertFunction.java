package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * assert function to assert an expression's value is true, otherwise throw an exception.
 * 
 * @since 5.0.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class AssertFunction extends AbstractFunction {

  /**
   * Assertion failiure.
   *
   * @author dennis(killme2008@gmail.com)
   *
   */
  public static final class AssertFailed extends RuntimeException {
    private static final long serialVersionUID = 1L;
  }

  @Override
  public String getName() {
    return "assert";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    boolean result = (boolean) arg1.getValue(env);
    if (!result) {
      throw new AssertFailed();
    }
    return AviatorNil.NIL;
  }

}
