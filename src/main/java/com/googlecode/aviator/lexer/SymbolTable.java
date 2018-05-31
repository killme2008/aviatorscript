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
package com.googlecode.aviator.lexer;

import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.lexer.token.Variable;


/**
 * Symbol table
 *
 * @author dennis
 *
 */
public class SymbolTable {

  private final Map<String, Variable> table = new HashMap<String, Variable>();


  public SymbolTable() {
    reserve("true", Variable.TRUE);
    reserve("false", Variable.FALSE);
    reserve("nil", Variable.NIL);
    reserve("lambda", Variable.LAMBDA);
  }


  /**
   * Reserve variable
   *
   * @param name
   * @param value
   */
  public void reserve(String name, Variable value) {
    table.put(name, value);
  }


  /**
   * Check variable has been reserved?
   *
   * @param name
   * @return
   */
  public boolean contains(String name) {
    return table.containsKey(name);
  }


  /**
   * Get symbold table
   *
   * @return
   */
  public Map<String, Variable> getTable() {
    return table;
  }


  /**
   * Get variable by name
   *
   * @param name
   * @return
   */
  public Variable getVariable(String name) {
    return table.get(name);
  }
}
