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
import java.util.Collection;
import java.util.Map;
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

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    Object first = arg1.getValue(env);
    if (first == null) {
      throw new NullPointerException("null seq");
    }
    Class<?> clazz = first.getClass();
    boolean contains = false;
    if (Collection.class.isAssignableFrom(clazz)) {
      Collection<?> seq = (Collection<?>) first;
      try {
        for (Object obj : seq) {
          if (new AviatorRuntimeJavaType(obj).compare(arg2, env) == 0) {
            contains = true;
            break;
          }
        }
      } catch (Exception e) {
        return AviatorBoolean.FALSE;
      }
    } else if (clazz.isArray()) {
      // Object[] seq = (Object[]) first;
      try {
        int length = Array.getLength(first);
        for (int i = 0; i < length; i++) {
          Object obj = Array.get(first, i);
          if (new AviatorRuntimeJavaType(obj).compare(arg2, env) == 0) {
            contains = true;
            break;
          }
        }
      } catch (Exception e) {
        return AviatorBoolean.FALSE;
      }
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a seq collection");
    }

    return AviatorBoolean.valueOf(contains);
  }


  public String getName() {
    return "include";
  }

}
