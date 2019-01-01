package com.googlecode.aviator;

import java.util.Map;
import com.googlecode.aviator.exception.ExpressionRuntimeException;
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
  public int compare(AviatorObject other, Map<String, Object> env) {
    throw new UnsupportedOperationException("Lambda function can't be compared.");
  }

  @Override
  public AviatorType getAviatorType() {
    return AviatorType.Lambda;
  }

  @Override
  public Object getValue(Map<String, Object> env) {
    return this;
  }

  @Override
  public AviatorObject call(Map<String, Object> env) {
    return getFunc(env).call(env);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    return getFunc(env).call(env, arg1);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return getFunc(env).call(env, arg1, arg2);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {
    return getFunc(env).call(env, arg1, arg2, arg3);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20);
  }

  @Override
  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {
    return getFunc(env).call(env, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10,
        arg11, arg12, arg13, arg14, arg15, arg16, arg17, arg18, arg19, arg20, args);
  }

  private final String name;

  RuntimeFunctionDelegator(String name) {
    this.name = name;
  }

  @Override
  public String getName() {
    return name;
  }

  private AviatorFunction getFunc(Map<String, Object> env) {
    Object val = env.get(name);
    if (val instanceof AviatorFunction) {
      return (AviatorFunction) val;
    }
    throw new ExpressionRuntimeException("Function not found: " + this.name);
  }
}
