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

import java.util.Map;

import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Function to make predicate for filter function
 * 
 * @author dennis
 * 
 */
public class SeqMakePredicateFunFunction implements AviatorFunction {
    private final String name;
    private final OperatorType opType;
    private final AviatorObject value;


    public SeqMakePredicateFunFunction(String name, OperatorType opType) {
        this(name, opType, null);
    }


    public SeqMakePredicateFunFunction(String name, OperatorType opType, AviatorObject value) {
        super();
        this.name = name;
        this.opType = opType;
        this.value = value;
    }


    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {

        // generate a temp function object as predicate
        AviatorFunction fun = new SeqPredicateFunction(name, opType, value == null ? args[0] : value);
        final String funName = name + "_tmp_" + System.nanoTime();
        env.put(funName, fun);
        return new AviatorJavaType(funName);
    }


    public String getName() {
        return name;
    }

}
