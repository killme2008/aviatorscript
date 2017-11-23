package com.googlecode.aviator.runtime.function.custom;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


public class AddFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return arg1.add(arg2, env);
  }


  @Override
  public String getName() {
    return "myadd";
  }

}
