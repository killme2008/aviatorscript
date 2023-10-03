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

import java.io.Serializable;
import java.util.HashMap;
import java.util.Map;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;


/**
 * Symbol table
 *
 * @author dennis
 *
 */
public class SymbolTable implements Serializable {

  private static final long serialVersionUID = -9019014977807517193L;

  private final Map<String, Variable> table = new HashMap<>();

  private static final Map<String, Variable> RESERVED = new HashMap<>();

  private static void reserveKeyword(final Variable v) {
    RESERVED.put(v.getLexeme(), v);
  }

  static {
    reserveKeyword(Variable.TRUE);
    reserveKeyword(Variable.FALSE);
    reserveKeyword(Variable.NIL);
    reserveKeyword(Variable.LAMBDA);
    reserveKeyword(Variable.FN);
    reserveKeyword(Variable.END);
    reserveKeyword(Variable.IF);
    reserveKeyword(Variable.ELSE);
    reserveKeyword(Variable.FOR);
    reserveKeyword(Variable.IN);
    reserveKeyword(Variable.RETURN);
    reserveKeyword(Variable.BREAK);
    reserveKeyword(Variable.CONTINUE);
    reserveKeyword(Variable.LET);
    reserveKeyword(Variable.WHILE);
    reserveKeyword(Variable.ELSIF);
    reserveKeyword(Variable.TRY);
    reserveKeyword(Variable.CATCH);
    reserveKeyword(Variable.FINALLY);
    reserveKeyword(Variable.THROW);
    reserveKeyword(Variable.NEW);
    reserveKeyword(Variable.USE);
  }

  public static boolean isReservedKeyword(final String name) {
    return RESERVED.containsKey(name);
  }

  public static boolean isReservedKeyword(final Variable v) {
    return isReservedKeyword(v.getLexeme());
  }


  /**
   * Check variable has been reserved?
   *
   * @param name
   * @return
   */
  public boolean isReserved(final String name) {
    return isReservedKeyword(name) || this.table.containsKey(name);
  }

  /**
   * Try to reserve key word, return the reserved variable if success, otherwise return itself.
   * 
   * @param var
   * @return
   */
  public static Variable tryReserveKeyword(final Variable var) {
    Variable reserve = RESERVED.get(var.getLexeme());
    return reserve != null ? reserve : var;
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

  public Variable reserve(final String lexeme) {
    if (isReserved(lexeme)) {
      return getVariable(lexeme);
    } else {
      final Variable var = new Variable(lexeme, 0, -1);
      this.table.put(lexeme, var);
      return var;
    }
  }

  public Token<?> reserve(final Variable variable) {
    String lexeme = variable.getLexeme();
    if (isReserved(lexeme)) {
      Variable v = getVariable(lexeme);
      if (v.getStartIndex() < 0) {
        return v;
      }
      variable.setLexeme(v.getLexeme());
      return variable;
    } else {
      final String name = lexeme;
      this.table.put(name, variable);
      return variable;
    }
  }
}
