package com.googlecode.aviator.runtime.function;

import java.util.Map;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

/**
 * Trace eval function.
 *
 * @author dennis
 *
 */
public class TraceFunction implements AviatorFunction {

  @Override
  public AviatorObject call() throws Exception {
    return this.call(Env.EMPTY_ENV);
  }

  @Override
  public void run() {
    this.call(Env.EMPTY_ENV);
  }

  private final AviatorFunction rawFunc;

  private TraceFunction(final AviatorFunction rawFunc) {
    super();
    this.rawFunc = rawFunc;
  }

  public static AviatorFunction wrapTrace(final AviatorFunction func) {
    return new TraceFunction(func);
  }

  private void traceResult(final Map<String, Object> env, final Object result) {
    RuntimeUtils.printlnTrace(env, "Result : " + result);
  }

  private void traceArgs(final Map<String, Object> env, final Object... args) {
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
    RuntimeUtils.printlnTrace(env, "Func   : " + getName() + "(" + sb.toString() + ")");
  }

  @Override
  public String getName() {
    return this.rawFunc.getName();
  }

  @Override
  public AviatorObject call(final Map<String, Object> env) {
    traceArgs(env);
    AviatorObject ret = this.rawFunc.call(env);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    traceArgs(env, arg1);
    AviatorObject ret = this.rawFunc.call(env, arg1);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    traceArgs(env, arg1, arg2);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {
    traceArgs(env, arg1, arg2, arg3);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4) {
    traceArgs(env, arg1, arg2, arg3, arg4);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    AviatorObject ret =
        this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    AviatorObject ret =
        this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
    AviatorObject ret =
        this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15, arg16);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18, final AviatorObject arg19) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18, arg19);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18, final AviatorObject arg19,
      final AviatorObject arg20) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    traceResult(env, ret);
    return ret;
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18, final AviatorObject arg19,
      final AviatorObject arg20, final AviatorObject... args) {
    traceArgs(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13,
        arg14, arg15, arg16, arg17, arg18, arg19, arg20, "...");
    AviatorObject ret = this.rawFunc.call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
        arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
    traceResult(env, ret);
    return ret;
  }
}
