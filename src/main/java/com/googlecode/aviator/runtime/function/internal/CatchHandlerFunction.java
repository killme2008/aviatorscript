package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * __catch_handler(fun, exception) to create a {@link CatchHandler}.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class CatchHandlerFunction extends AbstractFunction {

  /**
   *
   */
  private static final long serialVersionUID = 7314510329619948965L;

  @Override
  public String getName() {
    return "__catch_handler";
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    return new CatchHandler((AviatorFunction) arg1, ((AviatorJavaType) arg2).getName());
  }
}
