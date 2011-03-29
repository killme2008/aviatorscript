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
package com.googlecode.aviator.runtime.method;

import java.util.List;
import java.util.Map;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;


/**
 * A aviator method variable
 * 
 * @author dennis
 * 
 */
public class AviatorMethod extends AviatorObject {

    @Override
    public int compare(AviatorObject other, Map<String, Object> env) {
        throw new ExpressionRuntimeException("Could not compare method with " + other.desc(env));
    }


    @Override
    public AviatorType getAviatorType() {
        return AviatorType.Method;
    }


    @Override
    public Object getValue(Map<String, Object> env) {
        return this.methodName;
    }

    private final String methodName;
    private volatile AviatorFunction cachedFunction;


    public AviatorMethod(String methodName) {
        super();
        this.methodName = methodName;
    }


    public AviatorObject invoke(Map<String, Object> env, List<AviatorObject> list) {
        if (cachedFunction == null) {
            cachedFunction = (AviatorFunction) env.get(this.methodName);
        }
        if (cachedFunction == null) {
            throw new ExpressionRuntimeException("Could not find method named " + methodName);
        }
        final AviatorObject result = cachedFunction.call(env, list.toArray(new AviatorObject[list.size()]));
        return result == null ? AviatorNil.NIL : result;
    }

}
