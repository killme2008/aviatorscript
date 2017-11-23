package com.googlecode.aviator.runtime.function;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Abstract function to implement variadic arguments function.
 * 
 * @author dennis
 * @since 3.0.0
 * @Date 2016-12-09
 * 
 */
public abstract class AbstractVariadicFunction implements AviatorFunction {

  @Override
  public AviatorObject call(Map<String, Object> env) {
    return this.variadicCall(env);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    return this.variadicCall(env, arg1);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return variadicCall(env, arg1, arg2);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {
    return variadicCall(env, arg1, arg2, arg3);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4) {
    return variadicCall(env, arg1, arg2, arg3, arg4);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20) {
    return variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
        arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }


  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {
    if (args == null || args.length == 0) {
      return this.variadicCall(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
          arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
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
      return this.variadicCall(env, allArgs);
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
