package com.googlecode.aviator.utils;

import java.util.regex.Pattern;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.lexer.token.Variable;
import com.googlecode.aviator.runtime.function.internal.ReducerResult;
import com.googlecode.aviator.runtime.type.AviatorNil;

public class Constants {
  public static final String REDUCER_EMPTY_VAR = "__reducer_empty";
  public static final Variable ReducerEmptyVal = new Variable(REDUCER_EMPTY_VAR, 0, -1);
  public static final Variable IfReturnFn = new Variable("__if_callcc", 0, -1);
  public static final Token<?> ReducerBreakFn = new Variable("__reducer_break", 0, -1);
  public static final Token<?> ReducerContFn = new Variable("__reducer_cont", 0, -1);
  public static final Token<?> ReducerReturnFn = new Variable("__reducer_return", 0, -1);
  public static final Variable ReducerFn = new Variable("__reducer_callcc", 0, -1);
  public static final Variable WithMetaFn = new Variable("with_meta", 0, -1);
  public static final String PARAMS_META = "params";
  public static final String SCOPE_META = "newLexicalScope";
  public static final String INHERIT_ENV_META = "inheritEnv";
  public static final String INSTANCE_VAR = "__instance__";
  public static final String ENV_VAR = "__env__";
  public static final String FUNC_ARGS_VAR = "__args__";
  public static final String REDUCER_LOOP_VAR = "__reducer_loop";
  public static final String DEFINE_META = "define";
  public static final Variable REDUCER_LOOP = new Variable("__reducer_loop", 0, -1);
  public static final String NEWLINE = "\r\n";
  public static final String REQUIRE_FN = "require";
  public static final String LOAD_FN = "load";
  public static final Variable CATCH_HANDLER_VAR = new Variable("__catch_handler", 0, -1);
  public static final Variable SEQ_LIST_VAR = new Variable("seq.list", 0, -1);
  public static final Variable TRY_VAR = new Variable("__try", 0, -1);
  public static final Variable THROW_VAR = new Variable("__throw", 0, -1);
  public static final Variable NEW_VAR = new Variable("__new", 0, -1);
  public static final Variable THROWABLE_VAR = new Variable("Throwable", 0, -1);
  public static final String EXP_VAR = "__exp__";
  public static final Variable USE_VAR = new Variable("__use", 0, -1);
  public static final String INIT_META = "isInitialized";
  public static final String TYPE_META = "type";
  /**
   * Compiled-time metadata
   */

  // Whether string has interpolation point.
  public static final String INTER_META = "hasInterpolation";
  public static final String UNPACK_ARGS = "unpackingArgs";
  public static final String USE_CLASS_PKG = "useClassOrPkg";
  public static final Pattern SPLIT_PAT = Pattern.compile("\\.");

  // runtime metadata keys
  public static final String ARITIES_META = "arities";

  private Constants() {

  }
}
