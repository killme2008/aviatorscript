package com.googlecode.aviator.runtime.function.internal;

import java.util.Map;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.runtime.type.AviatorRuntimeJavaType;
import com.googlecode.aviator.runtime.type.AviatorType;
import com.googlecode.aviator.utils.Env;

/**
 * ReducerResult in looping.
 *
 * @author dennis(killme2008@gmail.com)
 * @since 5.0.0
 */
public class ReducerResult extends AviatorRuntimeJavaType {

  private static final long serialVersionUID = 8804868778622599851L;
  public final ReducerState state;
  public AviatorObject obj;

  public boolean isEmptyState() {
    return this.state == ReducerState.Empty;
  }

  public static ReducerResult withEmpty(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Empty, obj);
  }

  public static ReducerResult withCont(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Cont, obj);
  }

  public static ReducerResult withBreak(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Break, obj);
  }

  public static ReducerResult withReturn(final AviatorObject obj) {
    return new ReducerResult(ReducerState.Return, obj);
  }

  private ReducerResult(final ReducerState state, final AviatorObject obj) {
    super(obj);
    this.state = state;
    this.obj = obj;
    this.metadata = obj.getMetadata();
  }

  @Override
  public AviatorObject deref(final Map<String, Object> env) {
    this.obj = this.obj.deref(env);
    return this;
  }

  @Override
  public int innerCompare(final AviatorObject other, final Map<String, Object> env) {
    return this.obj.innerCompare(other, env);
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
    Object val = getValue(Env.EMPTY_ENV);
    if (val != this) {
      return "<Reducer, " + this.state.name() + ", " + val + ">";
    } else {
      return "<Reducer, " + this.state.name() + ", this>";
    }
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
