/**
 * Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 * This library is free software; you can redistribute it and/or modify it under the terms of the
 * GNU Lesser General Public License as published by the Free Software Foundation; either version
 * 2.1 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without
 * even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License along with this program;
 * if not, write to the Free Software Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.Comparator;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * sort(list, [comparator]) function to sort a java.util.List or array,return a sorted duplicate
 * object
 *
 * @author dennis
 *
 */
public class SeqSortFunction extends AbstractFunction {


  private static final long serialVersionUID = 8105967959099656098L;


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {

    Object first = arg1.getValue(env);
    if (first == null) {
      return AviatorNil.NIL;
    }
    Class<?> clazz = first.getClass();

    if (List.class.isAssignableFrom(clazz)) {
      List<?> list = (List<?>) first;
      Object[] a = list.toArray();
      Arrays.sort(a);
      return AviatorRuntimeJavaType.valueOf(Arrays.asList(a));
    } else if (clazz.isArray()) {
      int length = Array.getLength(first);
      Object[] dup = (Object[]) Array.newInstance(first.getClass().getComponentType(), length);
      for (int i = 0; i < length; i++) {
        dup[i] = Array.get(first, i);
      }
      // System.arraycopy(array, 0, dup, 0, dup.length);
      Arrays.sort(dup);
      return AviatorRuntimeJavaType.valueOf(dup);
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not an array or list.");
    }
  }

  @SuppressWarnings({"unchecked", "rawtypes"})
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {

    Object first = arg1.getValue(env);
    Comparator comparator = (Comparator) arg2.getValue(env);
    if (first == null) {
      return AviatorNil.NIL;
    }
    if (comparator == null) {
      throw new IllegalArgumentException("null comparator");
    }
    Class<?> clazz = first.getClass();

    if (List.class.isAssignableFrom(clazz)) {
      List<?> list = (List<?>) first;
      Object[] a = list.toArray();
      Arrays.sort(a, comparator);
      return AviatorRuntimeJavaType.valueOf(Arrays.asList(a));
    } else if (clazz.isArray()) {
      int length = Array.getLength(first);
      Object[] dup = (Object[]) Array.newInstance(first.getClass().getComponentType(), length);
      for (int i = 0; i < length; i++) {
        dup[i] = Array.get(first, i);
      }
      // System.arraycopy(array, 0, dup, 0, dup.length);
      Arrays.sort(dup, comparator);
      return AviatorRuntimeJavaType.valueOf(dup);
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not an array or list.");
    }
  }


  @Override
  public String getName() {
    return "sort";
  }

}
