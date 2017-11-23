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
package com.googlecode.aviator.runtime.function.system;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Map;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * println(obj) function to print object
 * 
 * @author dennis
 * 
 */
public class PrintFunction extends AbstractFunction {

  public String getName() {
    return "print";
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    System.out.print(arg1.getValue(env));

    return AviatorNil.NIL;
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {

    OutputStream out = (OutputStream) FunctionUtils.getJavaObject(arg1, env);
    PrintStream printStream = new PrintStream(out);
    printStream.print(arg2.getValue(env));

    return AviatorNil.NIL;
  }

}
