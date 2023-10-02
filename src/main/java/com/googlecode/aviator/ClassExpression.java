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

import java.util.List;
import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
import com.googlecode.aviator.lexer.SymbolTable;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.utils.Env;
import com.googlecode.aviator.utils.Reflector;

/**
 * Compiled expression,all generated class inherit this class
 *
 * @author dennis
 *
 */
public abstract class ClassExpression extends BaseExpression {

  private static final long serialVersionUID = -1335572034070560078L;

  protected transient byte[] classBytes;

  public byte[] getClassBytes() {
    return classBytes;
  }

  public void setClassBytes(byte[] classBytes) {
    this.classBytes = classBytes;
  }

  public ClassExpression(final AviatorEvaluatorInstance instance, final List<VariableMeta> vars,
      final SymbolTable symbolTable) {
    super(instance, vars, symbolTable);
  }

  @Override
  public Object executeDirectly(final Map<String, Object> env) {
    try {
      Object result = execute0((Env) env);
      if (RuntimeUtils.isTracedEval(env)) {
        RuntimeUtils.printlnTrace(env, "Result : " + result);
      }
      return result;
    } catch (ExpressionRuntimeException e) {
      throw e;
    } catch (Throwable t) {
      throw Reflector.sneakyThrow(t);
    }
  }

  public abstract Object execute0(Env env);

  /**
   * Get generated java class
   *
   * @return
   */
  public Class<?> getJavaClass() {
    return this.getClass();
  }
}
