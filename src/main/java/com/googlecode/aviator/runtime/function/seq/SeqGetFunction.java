package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.List;
import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * seq.get function to retrieve a element by index(for list) or key(for map).
 *
 * @since 4.1.2
 * @author dennis
 *
 */
public class SeqGetFunction extends AbstractFunction {


  private static final long serialVersionUID = -8707187642296260032L;

  @Override
  public String getName() {
    return "seq.get";
  }

  @SuppressWarnings("rawtypes")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {

    Object coll = arg1.getValue(env);
    Object key = arg2.getValue(env);
    if (coll == null) {
      throw new NullPointerException(
          "the collection of seq.get is null, which the value of key/index is `" + key + "`");
    }
    Class<?> clazz = coll.getClass();

    if (List.class.isAssignableFrom(clazz)) {
      if (!(key instanceof Number)) {
        throw new IllegalArgumentException(
            "Invalid index `" + key + "` for list,it's not a number.");
      }

      Object value = ((List) coll).get(((Number) key).intValue());
      return AviatorRuntimeJavaType.valueOf(value);
    } else if (Set.class.isAssignableFrom(clazz)) {
      if (((Set) coll).contains(key)) {
        return AviatorRuntimeJavaType.valueOf(key);
      } else {
        return AviatorNil.NIL;
      }
    } else if (Map.class.isAssignableFrom(clazz)) {
      Object value = ((Map) coll).get(key);
      return AviatorRuntimeJavaType.valueOf(value);
    } else if (clazz.isArray()) {
      if (!(key instanceof Number)) {
        throw new IllegalArgumentException(
            "Invalid index `" + key + "` for list,it's not a number.");
      }

      return AviatorRuntimeJavaType.valueOf(Array.get(coll, ((Number) key).intValue()));
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a collection.");
    }

  }

}
