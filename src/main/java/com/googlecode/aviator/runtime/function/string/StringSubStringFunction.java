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
package com.googlecode.aviator.runtime.function.string;

import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorString;


/**
 * string.substring(s1,s2) function
 * 
 * @author dennis
 * 
 */
public class StringSubStringFunction extends AbstractFunction {
  @Override
  public String getName() {
    return "string.substring";
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {

    String target = FunctionUtils.getStringValue(arg1, env);
    Number beginIndex = FunctionUtils.getNumberValue(arg2, env);
    Number endIndex = FunctionUtils.getNumberValue(arg3, env);
    return new AviatorString(target.substring(beginIndex.intValue(), endIndex.intValue()));

  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    String target = FunctionUtils.getStringValue(arg1, env);
    Number beginIndex = FunctionUtils.getNumberValue(arg2, env);

    return new AviatorString(target.substring(beginIndex.intValue()));

  }

}
