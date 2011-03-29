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
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * string.substring(s1,s2) function
 * 
 * @author dennis
 * 
 */
public class StringSubStringFunction implements AviatorFunction {
    public String getName() {
        return "string.substring";
    }


    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != 2 && args.length != 3) {
            throw new IllegalArgumentException("string.endsWith(string,beginIndex[,endIndex])");
        }
        String target = FunctionUtils.getStringValue(0, args, env);
        Number beginIndex = FunctionUtils.getNumberValue(1, args, env);
        switch (args.length) {
        case 2:

            return new AviatorString(target.substring(beginIndex.intValue()));
        case 3:
            Number endIndex = FunctionUtils.getNumberValue(2, args, env);
            return new AviatorString(target.substring(beginIndex.intValue(), endIndex.intValue()));

        }
        return null;
    }
}
