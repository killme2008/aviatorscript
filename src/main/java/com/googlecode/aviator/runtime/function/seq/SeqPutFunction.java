package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.utils.ArrayUtils;


/**
 * seq.put function to set a element value by index(for list) or key(for map).
 *
 * @since 5.0.0
 * @author dennis
 *
 */
public class SeqPutFunction extends AbstractFunction {


  private static final long serialVersionUID = -3135895014660784340L;

  @Override
  public String getName() {
    return "seq.put";
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
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


    if (List.class.isAssignableFrom(clazz)) {
      int index = ((Number) key).intValue();
      previousVal = ((List) coll).set(index, val);
    } else if (Map.class.isAssignableFrom(clazz)) {
      previousVal = ((Map) coll).put(key, val);
    } else if (clazz.isArray()) {
      int index = ((Number) key).intValue();
      previousVal = ArrayUtils.get(coll, index);
      ArrayUtils.set(coll, index, val);
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " can't put elements.");
    }
    return AviatorRuntimeJavaType.valueOf(previousVal);
  }

}
