package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * reverse(seq) to reverse an array or list in place.
 *
 * @author boyan(boyan@antfin.com)
 *
 */
public class SeqReverseFunction extends AbstractFunction {
  private static final long serialVersionUID = 784309776347529069L;

  private SeqReverseFunction() {}

  public static final SeqReverseFunction INSTANCE = new SeqReverseFunction();

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {

    Object first = arg1.getValue(env);
    if (first == null) {
      return AviatorNil.NIL;
    }
    Class<?> clazz = first.getClass();

    if (List.class.isAssignableFrom(clazz)) {
      List<?> list = (List<?>) first;
      Collections.reverse(list);
      return arg1;
    } else if (clazz.isArray()) {
      int length = Array.getLength(first);

      for (int i = 0; i < length / 2; i++) {
        Object temp = Array.get(first, i);
        Array.set(first, i, Array.get(first, length - 1 - i));
        Array.set(first, length - 1 - i, temp);
      }
      return arg1;
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not an array or list.");
    }
  }


  @Override
  public String getName() {
    return "reverse";
  }

}
