package com.googlecode.aviator.runtime.function.system;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * Cast value to string
 * 
 * @author dennis
 * @Date 2011-5-18
 * @since 1.1.1
 * 
 */
public class StrFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    final Object value = arg1.getValue(env);
    return new AviatorString(value == null ? "null" : value.toString());
  }


  public String getName() {
    return "str";
  }

}
