package com.googlecode.aviator.runtime.function.string;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * String.indexOf function
 * 
 * @author boyan
 * @Date 2011-7-13
 * 
 */
public class StringIndexOfFunction extends AbstractFunction {

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
