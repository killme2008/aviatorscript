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
package com.googlecode.aviator.runtime.function.system;

import java.io.OutputStream;
import java.io.PrintStream;
import java.util.Map;

import com.googlecode.aviator.runtime.function.FunctionUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * println(obj) function to print object with newline
 * 
 * @author dennis
 * 
 */
public class PrintlnFunction implements AviatorFunction {

    public String getName() {
        return "println";
    }


    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != 0 && args.length != 1 && args.length != 2) {
            throw new IllegalArgumentException("println([out],[obj])");
        }

        switch (args.length) {
        case 0:
            System.out.println();
            break;
        case 1:
            System.out.println(args[0].getValue(env));
            break;
        case 2:
            OutputStream out = (OutputStream) FunctionUtils.getJavaObject(0, args, env);
            PrintStream printStream = new PrintStream(out);
            printStream.println(args[1].getValue(env));
            break;
        }
        return AviatorNil.NIL;

    }

}
