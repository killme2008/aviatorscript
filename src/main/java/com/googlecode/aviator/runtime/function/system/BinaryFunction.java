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

import java.util.Map;

import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Binary function,includes +,-,*,/,%,!
 * 
 * @author dennis
 * 
 */
public class BinaryFunction implements AviatorFunction {
    private final OperatorType opType;


    public BinaryFunction(OperatorType opType) {
        super();
        this.opType = opType;
    }


    public String getName() {
        return opType.getToken();
    }


    public OperatorType getOpType() {
        return opType;
    }


    public AviatorObject call(Map<String, Object> env, AviatorObject... args) {
        if (args.length != this.opType.getOperandCount()) {
            throw new IllegalArgumentException(getName() + " has only " + opType.getOperandCount() + " arguments");
        }
        AviatorObject left = args[0];
        switch (opType) {
        case ADD:
            AviatorObject right = args[1];
            return left.add(right, env);
        case SUB:
            right = args[1];
            return left.sub(right, env);
        case MULT:
            right = args[1];
            return left.mult(right, env);
        case DIV:
            right = args[1];
            return left.div(right, env);
        case MOD:
            right = args[1];
            return left.mod(right, env);
        case NOT:
            return left.not(env);
        case NEG:
            return left.neg(env);
        default:
            throw new ExpressionRuntimeException(getName() + " is not a binary operation");

        }
    }

}
