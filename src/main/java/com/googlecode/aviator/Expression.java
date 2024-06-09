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

import java.io.Serializable;
import java.util.List;
import java.util.Map;


/**
 * A expression
 *
 * @author dennis
 *
 */
public interface Expression extends Serializable {

  /**
   * Execute an expression with an environment, returns the result.
   *
   * @param env Binding variable environment
   * @return the result of execution
   */
  Object execute(Map<String, Object> env);


  /**
   * Execute an expression with an empty environment, returns the result.
   *
   * @return the result of execution
   */
  Object execute();

  /**
   * Returns the source file name.
   *
   * @since 5.2.3
   * @return the source file name
   */
  public String getSourceFile();


  /**
   * Returns this expression's all uninitialized global variable names in order when using
   * AviatorEvaluator.EVAL mode, otherwise returns an empty list.
   *
   * @see com.googlecode.aviator.AviatorEvaluator#EVAL
   * @return
   */
  List<String> getVariableNames();


  /**
   * Returns this expression's all uninitialized global variable full names(contains dot) in order
   * when using AviatorEvaluator.EVAL mode, otherwise returns an empty list.
   *
   * @return
   */
  List<String> getVariableFullNames();

  /**
   * Created a faster env map(compare variable names by reference).The arguments should be a
   * sequence of pair <String, Object>.
   *
   * @param args
   * @return an env map
   */
  Map<String, Object> newEnv(final Object... args);

  /**
   * Adds the specified symbol to the symbol table and returns a reference to the unique symbol. If
   * the symbol already exists, the previous symbol reference is returned instead, in order
   * guarantee that symbol references remain unique.
   *
   * @param name The symbol name.
   */
  String addSymbol(String name);

  /**
   * Returns the function names in the expression when using AviatorEvaluator.EVAL mode, otherwise
   * returns an empty list.
   * 
   * @since 5.4.2
   * @return the function name list
   */
  List<String> getFunctionNames();
}
