package com.googlecode.aviator.runtime.function.string;

import java.lang.reflect.Array;
import java.util.Collection;
import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * string.join function
 * 
 * @author boyan
 * 
 */
public class StringJoinFunction extends AbstractFunction {
  @Override
  public String getName() {
    return "string.join";
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    Object target = arg1.getValue(env);
    if (target == null)
      throw new ExpressionRuntimeException("Could not replace with null string");
    return join(env, arg1, target, "");
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    Object target = arg1.getValue(env);
    String split = FunctionUtils.getStringValue(arg2, env);
    if (target == null)
      throw new ExpressionRuntimeException("Could not replace with null string");
    return join(env, arg1, target, split);
  }


  private AviatorObject join(Map<String, Object> env, AviatorObject arg1, Object target,
      String split) {
    Class<?> clazz = target.getClass();

    StringBuffer sb = new StringBuffer(50);
    if (Collection.class.isAssignableFrom(clazz)) {
      boolean wasFirst = true;
      for (Object obj : (Collection<?>) target) {
        wasFirst = append(sb, split, wasFirst, obj);
      }
    } else if (clazz.isArray()) {
      int length = Array.getLength(target);
      boolean wasFirst = true;
      for (int i = 0; i < length; i++) {
        Object obj = Array.get(target, i);
        wasFirst = append(sb, split, wasFirst, obj);
      }
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a seq");
    }
    return new AviatorString(sb.toString());
  }


  private boolean append(StringBuffer sb, String split, boolean wasFirst, Object obj) {
    String str = obj == null ? "null" : obj.toString();
    if (wasFirst) {
      sb.append(str);
      wasFirst = false;
    } else {
      sb.append(split).append(str);
    }
    return wasFirst;
  }

}
