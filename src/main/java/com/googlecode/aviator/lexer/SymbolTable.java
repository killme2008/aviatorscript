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

  private static final Map<String, Variable> RESERVED = new HashMap<>();

  static {
    RESERVED.put("true", Variable.TRUE);
    RESERVED.put("false", Variable.FALSE);
    RESERVED.put("nil", Variable.NIL);
    RESERVED.put("lambda", Variable.LAMBDA);
    RESERVED.put("end", Variable.END);
    RESERVED.put("if", Variable.IF);
    RESERVED.put("else", Variable.ELSE);
    RESERVED.put("for", Variable.FOR);
    RESERVED.put("in", Variable.IN);
    RESERVED.put("return", Variable.RETURN);
  }

  public static boolean isReserved(final Variable v) {
    return RESERVED.containsKey(v.getLexeme());
  }


  /**
   * Reserve variable
   *
   * @param name
   * @param value
   */
  public void reserve(final String name, final Variable value) {
    this.table.put(name, value);
  }


  /**
   * Check variable has been reserved?
   *
   * @param name
   * @return
   */
  public boolean contains(final String name) {
    return RESERVED.containsKey(name) || this.table.containsKey(name);
  }


  /**
   * Get variable by name
   *
   * @param name
   * @return
   */
  public Variable getVariable(final String name) {
    Variable var = RESERVED.get(name);
    return var != null ? var : this.table.get(name);
  }
}
