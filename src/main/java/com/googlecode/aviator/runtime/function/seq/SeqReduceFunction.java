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

import java.util.Collection;
import java.util.Map;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;


/**
 * reduce(col,fun,init) function to reduce seq with function and a initial value
 * value
 * 
 * @author dennis
 * 
 */
public class SeqReduceFunction implements AviatorFunction {

    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != 3) {
            throw new IllegalArgumentException("reduce(seq,fun,init)");
        }
        Object first = args[0].getValue(env);
        AviatorFunction fun = FunctionUtils.getFunction(1, args, env, 2);
        if (fun == null) {
            throw new ExpressionRuntimeException("There is no function named " + ((AviatorJavaType) args[1]).getName());
        }
        if (first == null) {
            throw new NullPointerException("null seq");
        }
        AviatorObject result = args[2];
        Class<?> clazz = first.getClass();

        if (Collection.class.isAssignableFrom(clazz)) {
            for (Object obj : (Collection<?>) first) {
                result = fun.call(env, result, new AviatorRuntimeJavaType(obj));
            }
        }
        else if (clazz.isArray()) {
            Object[] seq = (Object[]) first;
            for (Object obj : seq) {
                result = fun.call(env, result, new AviatorRuntimeJavaType(obj));
            }
        }
        else {
            throw new IllegalArgumentException(args[0].desc(env) + " is not a seq");
        }

        return result;
    }


    public String getName() {
        return "reduce";
    }

}
