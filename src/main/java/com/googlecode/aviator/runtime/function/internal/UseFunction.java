package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Constants;
import com.googlecode.aviator.utils.Env;

/**
 * function to implement import use
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class UseFunction extends AbstractFunction {

  private static final long serialVersionUID = 1710427343500339000L;



  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {

    if (arg1.getAviatorType() != AviatorType.JavaType) {
      throw new IllegalArgumentException("Can't import other aviator type except varaible");
    }

    final String sym = ((AviatorJavaType) arg1).getName();

    assert (env instanceof Env);

    ((Env) env).addSymbol(sym);
    return arg1;
  }



  @Override
  public String getName() {
    return Constants.USE_VAR.getLexeme();
  }

}
