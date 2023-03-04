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

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.util.List;
import java.util.Map;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.RuntimeUtils;


/**
 * A literal expression with a fixed result
 *
 * @author dennis
 *
 */
public class LiteralExpression extends BaseExpression {

  private static final long serialVersionUID = 7320801048592600557L;
  private Object result;

  public LiteralExpression(final AviatorEvaluatorInstance instance, final Object result,
      final List<VariableMeta> vars) {
    super(instance, vars, null);
    this.result = result;
  }

  @Override
  public Object executeDirectly(final Map<String, Object> env) {
    if (RuntimeUtils.isTracedEval(env)) {
      RuntimeUtils.printlnTrace(env, "Tracing: " + getExpression());
      RuntimeUtils.printlnTrace(env, "Result : " + getExpression());
    }
    return this.result;
  }

  @Override
  public String toString() {
    return "LiteralExpression [result=" + this.result + "]";
  }

  private void readObject(ObjectInputStream input) throws ClassNotFoundException, IOException {
    super.customReadObject(input);
    this.result = input.readObject();
  }

  private void writeObject(ObjectOutputStream output) throws IOException {
    super.customWriteObject(output);
    output.writeObject(this.result);
  }
}
