package com.googlecode.aviator.runtime.function.string;

import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * string.replace_all function
 * 
 * @author boyan
 * 
 */
public class StringReplaceAllFunction extends AbstractFunction {
  @Override
  public String getName() {
    return "string.replace_all";
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {
    String target = FunctionUtils.getStringValue(arg1, env);
    if (target == null)
      throw new ExpressionRuntimeException("Could not replace with null string");
    String regex = FunctionUtils.getStringValue(arg2, env);
    String replacement = FunctionUtils.getStringValue(arg3, env);
    return new AviatorString(target.replaceAll(regex, replacement));

  }

}
