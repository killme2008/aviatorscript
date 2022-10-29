package com.googlecode.aviator.runtime.function.seq;

import java.util.Collection;
import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeFunctionDelegator;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.Collector;
import com.googlecode.aviator.utils.ArrayUtils;

/**
 * seq.add function to add all elements in other sequence into this one.
 *
 * @author dennis
 * @since 5.3.3
 *
 */
public class SeqAddAllFunction extends AbstractFunction {

  private static final long serialVersionUID = -4406740199823615336L;

  @Override
  public String getName() {
    return "seq.add_all";
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {

    Object coll1 = arg1.getValue(env);
    Object coll2 = arg2.getValue(env);
    if (coll1 == null) {
      throw new NullPointerException("null seq");
    }
    if (coll2 == null) {
      return arg1;
    }
    Class<?> clazz = coll1.getClass();

    for (Object element : RuntimeUtils.seq(coll2, env)) {
      if (Collection.class.isAssignableFrom(clazz)) {
        ((Collection) coll1).add(element);
      } else if (Map.class.isAssignableFrom(clazz) && element instanceof Map.Entry) {
        Map.Entry entry = (Map.Entry) element;
        ((Map) coll1).put(entry.getKey(), entry.getValue());
      } else if (Collector.class.isAssignableFrom(clazz)) {
        ((Collector) coll1).add(element);
      } else {
        throw new IllegalArgumentException(arg1.desc(env) + " is not a collection or map.");
      }
    }
    return arg1;
  }
}
