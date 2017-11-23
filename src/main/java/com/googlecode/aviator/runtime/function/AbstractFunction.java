package com.googlecode.aviator.runtime.function;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorFunction;
import com.googlecode.aviator.runtime.type.AviatorObject;


/**
 * Abstract function implementation
 * 
 * @author boyan
 * @Date 2011-7-12
 * 
 */
public abstract class AbstractFunction implements AviatorFunction {
  public AviatorObject throwArity(int n) {
    String name = this.getName();
    throw new IllegalArgumentException("Wrong number of args (" + n + ") passed to: " + name);
  }


  public AviatorObject call(Map<String, Object> env) {
    return this.throwArity(0);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1) {
    return this.throwArity(1);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2) {
    return this.throwArity(2);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3) {
    return this.throwArity(3);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4) {
    return this.throwArity(4);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5) {
    return this.throwArity(5);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6) {
    return this.throwArity(6);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7) {
    return this.throwArity(7);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8) {
    return this.throwArity(8);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9) {
    return this.throwArity(9);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10) {
    return this.throwArity(10);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11) {
    return this.throwArity(11);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12) {
    return this.throwArity(12);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13) {
    return this.throwArity(13);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14) {
    return this.throwArity(14);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15) {
    return this.throwArity(15);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16) {
    return this.throwArity(16);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17) {
    return this.throwArity(17);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18) {
    return this.throwArity(18);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19) {
    return this.throwArity(19);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20) {
    return this.throwArity(20);
  }


  public AviatorObject call(Map<String, Object> env, AviatorObject arg1, AviatorObject arg2,
      AviatorObject arg3, AviatorObject arg4, AviatorObject arg5, AviatorObject arg6,
      AviatorObject arg7, AviatorObject arg8, AviatorObject arg9, AviatorObject arg10,
      AviatorObject arg11, AviatorObject arg12, AviatorObject arg13, AviatorObject arg14,
      AviatorObject arg15, AviatorObject arg16, AviatorObject arg17, AviatorObject arg18,
      AviatorObject arg19, AviatorObject arg20, AviatorObject... args) {
    return this.throwArity(21);
  }

}
