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
 * @author dennis(killme2008@gmail.com)
 *
 */
public class StringJoinFunction extends AbstractFunction {

  private static final long serialVersionUID = 8857093154788638443L;


  @Override
  public String getName() {
    return "string.join";
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    Object target = arg1.getValue(env);
    if (target == null) {
      throw new ExpressionRuntimeException("Could not replace with null string");
    }
    return join(env, arg1, target, "");
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    Object target = arg1.getValue(env);
    String split = FunctionUtils.getStringValue(arg2, env);
    if (target == null) {
      throw new ExpressionRuntimeException("Could not replace with null string");
    }
    return join(env, arg1, target, split);
  }


  private AviatorObject join(final Map<String, Object> env, final AviatorObject arg1,
      final Object target, final String split) {
    Class<?> clazz = target.getClass();

    StringBuilder sb = new StringBuilder(50);
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


  private boolean append(final StringBuilder sb, final String split, boolean wasFirst,
      final Object obj) {
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
