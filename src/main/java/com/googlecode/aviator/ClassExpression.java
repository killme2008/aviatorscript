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
import com.googlecode.aviator.runtime.LambdaFunctionBootstrap;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.function.LambdaFunction;
import com.googlecode.aviator.utils.Env;


/**
 * Compiled expression,all generated class inherit this class
 *
 * @author dennis
 *
 */
public abstract class ClassExpression extends BaseExpression {

  protected Map<String, LambdaFunctionBootstrap> lambdaBootstraps;


  public Map<String, LambdaFunctionBootstrap> getLambdaBootstraps() {
    return lambdaBootstraps;
  }

  public void setLambdaBootstraps(Map<String, LambdaFunctionBootstrap> lambdaBootstraps) {
    this.lambdaBootstraps = lambdaBootstraps;
  }

  public ClassExpression(AviatorEvaluatorInstance instance, List<String> varNames) {
    super(instance, varNames);
  }

  public LambdaFunction newLambda(Env env, String name) {
    LambdaFunctionBootstrap bootstrap = this.lambdaBootstraps.get(name);
    if (bootstrap == null) {
      throw new ExpressionRuntimeException("Lambda " + name + " not found");
    }
    return bootstrap.newInstance(env);
  }

  /*
   * (non-Javadoc)
   *
   * @see com.googlecode.aviator.IExpression#execute(java.util.Map)
   */
  @Override
  public Object execute(Map<String, Object> map) {
    if (map == null) {
      map = Collections.emptyMap();
    }
    Env env = newEnv(map);
    try {
      Object result = this.execute0(env);
      if (RuntimeUtils.isTracedEval(env)) {
        RuntimeUtils.printTrace(env, "Result : " + result);
      }
      return result;
    } catch (ExpressionRuntimeException e) {
      throw e;
    } catch (Throwable e) {
      throw new ExpressionRuntimeException("Execute expression error", e);
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
