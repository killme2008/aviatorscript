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
import java.util.Set;


/**
 * A expression
 * 
 * @author dennis
 * 
 */
public interface Expression {

  /**
   * Execute expression with environment
   * 
   * @param env Binding variable environment
   * @return
   */
  public abstract Object execute(Map<String, Object> env);


  /**
   * Execute expression with empty environment
   * 
   * @return
   */
  public abstract Object execute();


  /**
   * Returns this expression's all variable names in order when using AviatorEvaluator.EVAL
   * mode,else returns empty set
   * 
   * @see com.googlecode.aviator.AviatorEvaluator#EVAL
   * @return
   */
  public List<String> getVariableNames();


  /**
   * Returns this expression's all variable full names in order when using AviatorEvaluator.EVAL
   * mode,else returns empty set
   * 
   * @return
   */
  public List<String> getVariableFullNames();

}
