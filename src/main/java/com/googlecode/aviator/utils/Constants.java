package com.googlecode.aviator.utils;

import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;

public class Constants {
  public static final Variable ReducerEmptyVal = new Variable("__reducer_empty", -1);
  public static final Variable IfReturnFn = new Variable("__if_return", -1);
  public static final Token<?> ReducerBreakFn = new Variable("__reducer_break", -1);
  public static final Token<?> ReducerContFn = new Variable("__reducer_cont", -1);
  public static final Token<?> ReducerReturnFn = new Variable("__reducer_return", -1);
  public static final Variable ReducerFn = new Variable("__reducer", -1);
  public static final String PARAMS_META = "params";
  public static final String SCOPE_META = "newLexicalScope";
  public static final String INSTANCE_VAR = "__instance__";
  public static final String ENV_VAR = "__env__";
  public static final String FUNC_ARGS_VAR = "__args__";

  private Constants() {

  }
}
