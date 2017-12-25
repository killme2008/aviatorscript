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

import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.token.OperatorType;
import com.googlecode.aviator.runtime.function.AbstractFunction;
import com.googlecode.aviator.runtime.op.OperationRuntime;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Binary function,includes +,-,*,/,%,!
 *
 * @author dennis
 *
 */
public class BinaryFunction extends AbstractFunction {
  private final OperatorType opType;


  public BinaryFunction(OperatorType opType) {
    super();
    this.opType = opType;
  }


  @Override
  public String getName() {
    return this.opType.getToken();
  }


  public OperatorType getOpType() {
    return this.opType;
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return OperationRuntime.eval(arg1, arg2, env, this.opType);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    AviatorObject left = arg1;
    switch (this.opType) {
      case BIT_AND:
      case BIT_OR:
      case BIT_XOR:
      case ADD:
      case SUB:
      case MULT:
      case DIV:
      case MOD:
        return this.throwArity(1);
      case NOT:
      case NEG:
        return OperationRuntime.eval(left, null, env, this.opType);
      default:
        throw new ExpressionRuntimeException("Invalid binary operator");

    }
  }

}
