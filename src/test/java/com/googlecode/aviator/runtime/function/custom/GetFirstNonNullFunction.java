package com.googlecode.aviator.runtime.function.custom;

import com.googlecode.aviator.runtime.function.AbstractVariadicFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;
import java.util.Map;


/**
 * From user's issue report https://github.com/killme2008/aviator/issues/12
 * 
 * @author dennis
 *
 */
public class GetFirstNonNullFunction extends AbstractVariadicFunction {

  public AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args) {
    if (args != null) {
      for (AviatorObject arg : args) {
        if (arg.getValue(env) != null) {
          return arg;
        }
      }
    }
    return new AviatorString(null);
  }


  @Override
  public String getName() {
    return "getFirstNonNull";
  }

}
