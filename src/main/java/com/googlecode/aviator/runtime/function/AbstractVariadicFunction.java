package com.googlecode.aviator.runtime.function;

import java.util.Map;
import com.googlecode.aviator.exception.CompareNotSupportedException;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Env;


/**
 * Abstract function to implement variadic arguments function.
 *
 * @author dennis
 * @since 3.0.0
 * @Date 2016-12-09
 *
 */
public abstract class AbstractVariadicFunction extends AviatorObject implements AviatorFunction {


  private static final long serialVersionUID = -5939898720859638046L;

  @Override
  public AviatorObject call() throws Exception {
    return this.call(Env.EMPTY_ENV);
  }

  @Override
  public void run() {
    this.call(Env.EMPTY_ENV);
  }


  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Lambda;
  }

  @Override
  public String desc(final Map<String, Object> env) {
    return "<" + getAviatorType() + ", " + getName() + ">";
  }


  @Override
  public Object getValue(final Map<String, Object> env) {
    return this;
  }

  @Override
  public int innerCompare(final AviatorObject other, final Map<String, Object> env) {
    throw new CompareNotSupportedException("Lambda function can't be compared.");
  }

  @Override
  public AviatorObject call(final Map<String, Object> env) {
    return variadicCall(env);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return variadicCall(env, arg1);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    return variadicCall(env, arg1, arg2);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {
    return variadicCall(env, arg1, arg2, arg3);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4) {
    return variadicCall(env, arg1, arg2, arg3, arg4);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }


  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18, final AviatorObject arg19) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
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
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
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
    if (args == null || args.length == 0) {
      return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
          arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
    } else {
      AviatorObject[] allArgs = new AviatorObject[20 + args.length];
      allArgs[0] = arg1;
      allArgs[1] = arg2;
      allArgs[2] = arg3;
      allArgs[3] = arg4;
      allArgs[4] = arg5;
      allArgs[5] = arg6;
      allArgs[6] = arg7;
      allArgs[7] = arg8;
      allArgs[8] = arg9;
      allArgs[9] = arg10;
      allArgs[10] = arg11;
      allArgs[11] = arg12;
      allArgs[12] = arg13;
      allArgs[13] = arg14;
      allArgs[14] = arg15;
      allArgs[15] = arg16;
      allArgs[16] = arg17;
      allArgs[17] = arg18;
      allArgs[18] = arg19;
      allArgs[19] = arg20;
      System.arraycopy(args, 0, allArgs, 20, args.length);
      return variadicCall(env, allArgs);
    }
  }


  /**
   * Call with variadic arguments.The subclass must implement this method.
   *
   * @since 3.0.0
   * @param env
   * @param args
   * @return
   */
  public abstract AviatorObject variadicCall(Map<String, Object> env, AviatorObject... args);

}
