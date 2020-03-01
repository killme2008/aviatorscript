package com.googlecode.aviator.parser;

import com.googlecode.aviator.code.CodeGenerator;
import com.googlecode.aviator.lexer.SymbolTable;


public interface Parser {

  SymbolTable getSymbolTable();

  CodeGenerator getCodeGenerator();

  void setCodeGenerator(CodeGenerator codeGenerator);

  ScopeInfo enterScope(boolean inForLoop);

  void restoreScope(ScopeInfo info);

}
