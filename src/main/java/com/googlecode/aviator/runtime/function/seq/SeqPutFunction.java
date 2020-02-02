package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.TypeUtils;


/**
 * seq.put function to set a element value by index(for list) or key(for map).
 *
 * @since 4.1.2
 * @author dennis
 *
 */
public class SeqPutFunction extends AbstractFunction {

  @Override
  public String getName() {
    return "seq.put";
  }

  @SuppressWarnings("rawtypes")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {

    Object coll = arg1.getValue(env);
    Object key = arg2.getValue(env);
    Object val = arg3.getValue(env);
    if (coll == null) {
      throw new NullPointerException("null seq");
    }
    Class<?> clazz = coll.getClass();

    Object previousVal = null;

    int index = ((Number) key).intValue();
    if (List.class.isAssignableFrom(clazz)) {
      if (!TypeUtils.isLong(key)) {
        throw new IllegalArgumentException(
            "Invalid index `" + key + "` for list,it's not a integer.");
      }

      previousVal = ((List) coll).set(index, val);
    } else if (Map.class.isAssignableFrom(clazz)) {
      previousVal = ((Map) coll).put(key, val);
    } else if (clazz.isArray()) {
      if (!TypeUtils.isLong(key)) {
        throw new IllegalArgumentException(
            "Invalid index `" + key + "` for array,it's not a integer.");
      }
      previousVal = Array.get(coll, index);
      Array.set(coll, index, val);
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a collection.");
    }
    return new AviatorRuntimeJavaType(previousVal);
  }

}
