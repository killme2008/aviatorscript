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

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorBoolean;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * A predicate function
 * 
 * @author dennis
 * 
 */
public class SeqPredicateFunction implements AviatorFunction {
    private final String name;
    private final OperatorType opType;
    private final AviatorObject value;


    public SeqPredicateFunction(String name, OperatorType opType, AviatorObject value) {
        super();
        this.name = name;
        this.opType = opType;
        this.value = value;
    }


    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != opType.getOperandCount() - 1) {
            throw new IllegalArgumentException(getName() + " only have " + opType.getOperandCount() + " operands");
        }
        switch (opType) {
        case EQ:
            return args[0].compare(value, env) == 0 ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
        case NEQ:
            return args[0].compare(value, env) != 0 ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
        case LT:
            return args[0].compare(value, env) < 0 ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
        case LE:
            return args[0].compare(value, env) <= 0 ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
        case GE:
            return args[0].compare(value, env) >= 0 ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
        case GT:
            return args[0].compare(value, env) > 0 ? AviatorBoolean.TRUE : AviatorBoolean.FALSE;
        default:
            throw new ExpressionRuntimeException(getName() + " is not a relation operator");
        }
    }


    public String getName() {
        return name;
    }

}
