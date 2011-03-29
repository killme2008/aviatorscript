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
package com.googlecode.aviator.runtime.function.string;

import java.util.Map;

import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * string.endsWith(s1,s2) function
 * @author dennis
 *
 */
public class StringEndsWithFunction implements AviatorFunction {
    public String getName() {
        return "string.endsWith";
    }


    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != 2) {
            throw new IllegalArgumentException("string.endsWith(string,string|char)");
        }
        String target = FunctionUtils.getStringValue(0, args, env);
        String param = FunctionUtils.getStringValue(1, args, env);
        return target.endsWith(param)? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
    }
}
