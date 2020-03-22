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
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.type.AviatorLong;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.Range;


/**
 * count(seq) to get seq's size
 *
 * @author dennis
 *
 */
public class SeqCountFunction extends AbstractFunction {

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    Object value = arg1.getValue(env);
    if (value == null) {
      throw new NullPointerException("null seq");
    }
    Class<?> clazz = value.getClass();

    int size = -1;
    if (Collection.class.isAssignableFrom(clazz)) {
      Collection<?> col = (Collection<?>) value;
      size = col.size();
    } else if (Map.class.isAssignableFrom(clazz)) {
      size = ((Map) value).size();
    } else if (CharSequence.class.isAssignableFrom(clazz)) {
      size = ((CharSequence) value).length();
    } else if (clazz.isArray()) {
      size = Array.getLength(value);
    } else if (Range.class.isAssignableFrom(clazz)) {
      size = ((Range) value).size();
    } else {
      size = 0;
      for (Object e : RuntimeUtils.seq(value)) {
        size++;
      }
    }
    return AviatorLong.valueOf(size);
  }


  @Override
  public String getName() {
    return "count";
  }

}
