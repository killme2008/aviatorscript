package com.googlecode.aviator.runtime.function.string;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * String.indexOf function
 * 
 * @author dennis(killme2008@gmail.com)
 * @Date 2011-7-13
 * 
 */
public class StringIndexOfFunction extends AbstractFunction {


  private static final long serialVersionUID = 4497808043956407590L;


  @Override
  public String getName() {
    return "string.indexOf";
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    String target = FunctionUtils.getStringValue(arg1, env);
    String param = FunctionUtils.getStringValue(arg2, env);
    return AviatorLong.valueOf(target.indexOf(param));
  }

}
