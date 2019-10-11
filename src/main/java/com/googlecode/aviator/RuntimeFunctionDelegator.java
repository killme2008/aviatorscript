package com.googlecode.aviator;

import java.util.Map;
import com.googlecode.aviator.exception.FunctionNotFoundException;
import com.googlecode.aviator.runtime.function.system.ConstantFunction;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;

/**
 * Runtime function delegator
 *
 * @author dennis
 *
 */
final class RuntimeFunctionDelegator extends AviatorObject implements AviatorFunction {



  @Override
  public int compare(final AviatorObject other, final Map<String, Object> env) {
    throw new UnsupportedOperationException("Lambda function can't be compared.");
  }

  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Lambda;
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    return this;
  }

  @Override
  public AviatorObject call(final Map<String, Object> env) {
    return getFunc(env).call(env);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1) {
    return getFunc(env, arg1).call(env, arg1);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2) {
    return getFunc(env, arg1, arg2).call(env, arg1, arg2);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3) {
    return getFunc(env, arg1, arg2, arg3).call(env, arg1, arg2, arg3);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4) {
    return getFunc(env, arg1, arg2, arg3, arg4).call(env, arg1, arg2, arg3, arg4);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5).call(env, arg1, arg2, arg3, arg4, arg5);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6).call(env, arg1, arg2, arg3, arg4, arg5,
        arg6);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7).call(env, arg1, arg2, arg3, arg4,
        arg5, arg6, arg7);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8).call(env, arg1, arg2, arg3,
        arg4, arg5, arg6, arg7, arg8);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9).call(env, arg1, arg2,
        arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10).call(env, arg1,
        arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11)
        .call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12)
        .call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
            arg13);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11,
            arg12, arg13, arg14);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14, arg15).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
            arg11, arg12, arg13, arg14, arg15);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14, arg15, arg16).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
            arg10, arg11, arg12, arg13, arg14, arg15, arg16);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14, arg15, arg16, arg17).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8,
            arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14, arg15, arg16, arg17, arg18).call(env, arg1, arg2, arg3, arg4, arg5, arg6,
            arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }

  @Override
  public AviatorObject call(final Map<String, Object> env, final AviatorObject arg1,
      final AviatorObject arg2, final AviatorObject arg3, final AviatorObject arg4,
      final AviatorObject arg5, final AviatorObject arg6, final AviatorObject arg7,
      final AviatorObject arg8, final AviatorObject arg9, final AviatorObject arg10,
      final AviatorObject arg11, final AviatorObject arg12, final AviatorObject arg13,
      final AviatorObject arg14, final AviatorObject arg15, final AviatorObject arg16,
      final AviatorObject arg17, final AviatorObject arg18, final AviatorObject arg19) {
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14, arg15, arg16, arg17, arg18, arg19).call(env, arg1, arg2, arg3, arg4, arg5,
            arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18,
            arg19);
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
    return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
        arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20).call(env, arg1, arg2, arg3, arg4,
            arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17,
            arg18, arg19, arg20);
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
      return getFunc(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12,
          arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20).call(env, arg1, arg2, arg3, arg4,
              arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17,
              arg18, arg19, arg20);
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
      return getFunc(env, allArgs).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9,
          arg10, arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
    }
  }

  private final String name;

  private final FunctionMissing functionMissing;

  RuntimeFunctionDelegator(final String name, final FunctionMissing functionMissing) {
    this.name = name;
    this.functionMissing = functionMissing;
  }

  @Override
  public String getName() {
    return this.name;
  }

  private AviatorFunction getFunc(final Map<String, Object> env, final AviatorObject... args) {
    Object val = env.get(this.name);
    if (val instanceof AviatorFunction) {
      return (AviatorFunction) val;
    }
    if (this.functionMissing != null) {
      return new ConstantFunction(this.name,
          this.functionMissing.onFunctionMissing(this.name, env, args));
    }
    throw new FunctionNotFoundException("Function not found: " + this.name);
  }
}
