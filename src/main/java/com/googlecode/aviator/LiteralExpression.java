package com.googlecode.aviator;

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
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.runtime.op.OperationRuntime;


/**
 * A literal expression with a fixed result
 *
 * @author dennis
 *
 */
public class LiteralExpression extends BaseExpression {

  private final Object result;


  public LiteralExpression(Object result, List<String> varNames) {
    super(varNames);
    this.result = result;
  }


  @Override
  public Object execute(Map<String, Object> env) {
    if (OperationRuntime.isTracedEval()) {
      OperationRuntime.printTrace("Tracing: " + this.getExpression());
    }
    return this.result;
  }

}
