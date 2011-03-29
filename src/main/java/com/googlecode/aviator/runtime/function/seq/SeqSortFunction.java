/**
 *  Copyright (C) 2010 dennis zhuang (killme2008@gmail.com)
 *
 *  This library is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU Lesser General Public License as published
 *  by the Free Software Foundation; either version 2.1 of the License, or
 *  (at your option) any later version.
 *
 *  This library is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU Lesser General Public License for more details.
 *
 *  You should have received a copy of the GNU Lesser General Public License
 *  along with this program; if not, write to the Free Software
 *  Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 **/
package com.googlecode.aviator.runtime.function.seq;

import java.lang.reflect.Array;
import java.util.Arrays;
import java.util.List;
import java.util.Map;

import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * sort(list) function to sort java.util.List or array,return a sorted duplicate
 * object
 * 
 * @author dennis
 * 
 */
public class SeqSortFunction implements AviatorFunction {

    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != 1) {
            throw new IllegalArgumentException("sort(seq)");
        }

        Object first = args[0].getValue(env);
        if (first == null) {
            throw new NullPointerException("null seq");
        }
        Class<?> clazz = first.getClass();

        if (List.class.isAssignableFrom(clazz)) {
            List<?> list = (List<?>) first;
            Object[] a = list.toArray();
            Arrays.sort(a);
            return new AviatorRuntimeJavaType(Arrays.asList(a));
        }
        else if (clazz.isArray()) {
            Object[] array = (Object[]) first;
            Object[] dup = (Object[]) Array.newInstance(array.getClass().getComponentType(), array.length);
            System.arraycopy(array, 0, dup, 0, dup.length);
            Arrays.sort(dup);
            return new AviatorRuntimeJavaType(dup);
        }
        else {
            throw new IllegalArgumentException(args[0].desc(env) + " is not a seq");
        }
    }


    public String getName() {
        return "sort";
    }

}
