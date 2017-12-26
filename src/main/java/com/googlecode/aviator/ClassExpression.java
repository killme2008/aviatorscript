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
package com.googlecode.aviator;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.runtime.op.OperationRuntime;


/**
 * Compiled expression,all generated class inherit this class
 *
 * @author dennis
 *
 */
public abstract class ClassExpression extends BaseExpression {

  public ClassExpression(List<String> varNames) {
    super(varNames);
  }


  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#execute(java.util.Map)
   */
  @Override
  public Object execute(Map<String, Object> env) {
    if (env == null) {
      env = Collections.emptyMap();
    }

    if (OperationRuntime.isTracedEval()) {
      OperationRuntime.printTrace("Tracing: " + this.getExpression());
    }
    try {
      return this.execute0(env);
    } catch (ExpressionRuntimeException e) {
      throw e;
    } catch (Throwable e) {
      throw new ExpressionRuntimeException("Execute expression error", e);
    }

  }


  public abstract Object execute0(Map<String, Object> env);


  /**
   * Get generated java class
   *
   * @return
   */
  public Class<?> getJavaClass() {
    return this.getClass();
  }
}
