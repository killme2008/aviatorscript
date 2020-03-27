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

import java.util.Map;
import java.util.Set;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * include(seq,obj) function to check if seq contains object
 *
 * @author dennis
 *
 */
public class SeqIncludeFunction extends AbstractFunction {

  @SuppressWarnings("rawtypes")
  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    Object first = arg1.getValue(env);
    if (first == null) {
      throw new NullPointerException("null seq");
    }
    Class<?> clazz = first.getClass();
    boolean contains = false;
    if (Set.class.isAssignableFrom(clazz)) {
      contains = ((Set) first).contains(arg2.getValue(env));
    } else {
      try {
        for (Object obj : RuntimeUtils.seq(first, env)) {
          if (AviatorRuntimeJavaType.valueOf(obj).compare(arg2, env) == 0) {
            contains = true;
            break;
          }
        }
      } catch (Exception e) {
        RuntimeUtils.printStackTrace(env, e);
        return AviatorBoolean.FALSE;
      }
    }
    return AviatorBoolean.valueOf(contains);

  }


  @Override
  public String getName() {
    return "include";
  }

}
