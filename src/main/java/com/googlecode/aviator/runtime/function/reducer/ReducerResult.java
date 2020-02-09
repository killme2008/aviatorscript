package com.googlecode.aviator.runtime.function.reducer;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorType;

/**
 * ReducerResult in looping.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.0.0
 */
public class ReducerResult extends AviatorObject {
  public final ReducerState state;
  public final AviatorObject obj;


  public static ReducerResult withCont(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Cont, obj);
  }

  public static ReducerResult withBreak(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Break, obj);
  }

  public static ReducerResult withReturn(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Return, obj);
  }

  ReducerResult(final ReducerState state) {
    super();
    this.state = state;
    this.obj = this;
  }


  ReducerResult(final ReducerState state, final AviatorObject obj) {
    super();
    this.state = state;
    this.obj = obj;
  }

  @Override
  public int compare(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.compare(other, env);
  }

  @Override
  public AviatorType getAviatorType() {
    return this.obj.getAviatorType();
  }

  @Override
  public Object getValue(final Map<String, Object> env) {
    if (this.obj == this) {
      return this;
    }
    return this.obj.getValue(env);
  }

  @Override
  public String toString() {
    return this.obj.toString();
  }

  @Override
  public boolean isNull(final Map<String, Object> env) {
    return this.obj.isNull(env);
  }

  @Override
  public int hashCode() {
    return this.obj.hashCode();
  }

  @Override
  public AviatorObject match(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.match(other, env);
  }

  @Override
  public AviatorObject neg(final Map<String, Object> env) {
    return this.obj.neg(env);
  }

  @Override
  public AviatorObject setValue(final AviatorObject value, final Map<String, Object> env) {
    return this.obj.setValue(value, env);
  }

  @Override
  public AviatorObject not(final Map<String, Object> env) {
    return this.obj.not(env);
  }

  @Override
  public String desc(final Map<String, Object> env) {
    return this.obj.desc(env);
  }

  @Override
  public AviatorObject add(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.add(other, env);
  }

  @Override
  public AviatorObject bitAnd(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.bitAnd(other, env);
  }

  @Override
  public AviatorObject bitOr(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.bitOr(other, env);
  }

  @Override
  public AviatorObject bitXor(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.bitXor(other, env);
  }

  @Override
  public AviatorObject shiftRight(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.shiftRight(other, env);
  }

  @Override
  public boolean equals(final Object obj) {
    return this.obj.equals(obj);
  }

  @Override
  public AviatorObject shiftLeft(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.shiftLeft(other, env);
  }

  @Override
  public AviatorObject unsignedShiftRight(final AviatorObject other,
      final Map<String, Object> env) {
    return this.obj.unsignedShiftRight(other, env);
  }

  @Override
  public AviatorObject bitNot(final Map<String, Object> env) {
    return this.obj.bitNot(env);
  }

  @Override
  public AviatorObject sub(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.sub(other, env);
  }

  @Override
  public AviatorObject mod(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.mod(other, env);
  }

  @Override
  public AviatorObject div(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.div(other, env);
  }

  @Override
  public AviatorObject mult(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.mult(other, env);
  }

  @Override
  public Number numberValue(final Map<String, Object> env) {
    return this.obj.numberValue(env);
  }

  @Override
  public String stringValue(final Map<String, Object> env) {
    return this.obj.stringValue(env);
  }

  @Override
  public boolean booleanValue(final Map<String, Object> env) {
    return this.obj.booleanValue(env);
  }

  @Override
  public AviatorObject getElement(final Map<String, Object> env, final AviatorObject indexObject) {
    return this.obj.getElement(env, indexObject);
  }

}
