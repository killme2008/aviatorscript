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
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * count(seq) to get seq's size
 * 
 * @author dennis
 * 
 */
public class SeqCountFunction extends AbstractFunction {

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    Object value = arg1.getValue(env);
    if (value == null) {
      throw new NullPointerException("null seq");
    }
    Class<?> clazz = value.getClass();

    int size = -1;
    if (Collection.class.isAssignableFrom(clazz)) {
      Collection<?> col = (Collection<?>) value;
      size = col.size();
    } else if (clazz.isArray()) {
      size = Array.getLength(value);
    } else {
      throw new IllegalArgumentException(arg1.desc(env) + " is not a seq collection");
    }
    return AviatorLong.valueOf(size);
  }


  public String getName() {
    return "count";
  }

}
