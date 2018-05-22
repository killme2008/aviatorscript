package com.googlecode.aviator.runtime.function;

import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;

/**
 * Trace eval function.
 *
 * @author dennis
 *
 */
public class TraceFunction implements AviatorFunction {

  private AviatorFunction rawFunc;



  private TraceFunction(AviatorFunction rawFunc) {
    super();
    this.rawFunc = rawFunc;
  }

  public static AviatorFunction wrapTrace(AviatorFunction func) {
    return new TraceFunction(func);
  }

  private void trace(Map<String, Object> env, Object... args) {
    StringBuilder sb = new StringBuilder();
    boolean wasFirst = true;
    for (Object arg : args) {
      if (wasFirst) {
        wasFirst = false;
      } else {
        sb.append(",");
      }
      if (arg instanceof String) {
        sb.append(arg.toString());
      } else {
        sb.append(((AviatorObject) arg).desc(env));
      }
    }
    RuntimeUtils.printTrace(env, "Func   : " + this.getName() + "(" + sb.toString() + ")");
  }

  @Override
  public String getName() {
    return this.rawFunc.getName();
  }

  @Override
  public AviatorObject call(Map<String, Object> env) {
    trace(env);
    return this.rawFunc.call(env);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    trace(env, arg1);
    return this.rawFunc.call(env, arg1);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    trace(env, arg1, arg2);
    return rawFunc.call(env, arg1, arg2);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {
    trace(env, arg1, arg2, arg3);
    return rawFunc.call(env, arg1, arg2, arg3);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4) {
    trace(env, arg1, arg2, arg3, arg4);
    return rawFunc.call(env, arg1, arg2, arg3, arg4);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
    trace(env, arg1, arg2, arg3, arg4, arg5);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18, arg19);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    return rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {
    trace(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18, arg19, arg20, "...");
    return this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
  }
}
