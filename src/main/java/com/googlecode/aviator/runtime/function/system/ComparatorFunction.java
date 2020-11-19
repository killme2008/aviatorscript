package com.googlecode.aviator.runtime.function.system;

import java.util.Comparator;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;

/**
 * Returns an implementation of java.util.Comparator based upon pred function.
 *
 * @since 5.2.0
 * @author dennis(killme2008@gmail.com)
 *
 */
public class ComparatorFunction extends AbstractFunction {

  private ComparatorFunction() {}

  public static final ComparatorFunction INSTANCE = new ComparatorFunction();


  private static final long serialVersionUID = 6748727841901719306L;

  @Override
  public String getName() {
    return "comparator";
  }

  @SuppressWarnings("rawtypes")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    final AviatorFunction func = FunctionUtils.getFunction(arg1, env, 2);

    return AviatorRuntimeJavaType.valueOf(new Comparator() {

      @Override
      public int compare(final Object o1, final Object o2) {
        AviatorObject x = AviatorRuntimeJavaType.valueOf(o1);
        AviatorObject y = AviatorRuntimeJavaType.valueOf(o2);

        if ((boolean) func.call(env, x, y).getValue(env)) {
          return -1;
        } else if ((boolean) func.call(env, y, x).getValue(env)) {
          return 1;
        } else {
          return 0;
        }
      }

    });
  }

}
