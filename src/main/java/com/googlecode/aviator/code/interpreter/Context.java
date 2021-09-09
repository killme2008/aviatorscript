package com.googlecode.aviator.code.interpreter;

import java.util.ArrayDeque;
import java.util.Collections;
import java.util.List;
import com.googlecode.aviator.InterpretExpression;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

/**
 * Eval IR context.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class Context {
  private final ArrayDeque<AviatorObject> operands = new ArrayDeque<>();
  private IR pc;
  private int pcIndex = -1;
  private List<IR> instruments = Collections.emptyList();
  private final Env env;
  private final InterpretExpression expression;

  public Context(final InterpretExpression exp, final List<IR> instruments, final Env env) {
    this.expression = exp;
    this.instruments = instruments;
    this.env = env;
    next();
  }

  public InterpretExpression getExpression() {
    return this.expression;
  }

  public Env getEnv() {
    return this.env;
  }

  public boolean next() {
    this.pcIndex++;
    if (this.pcIndex < this.instruments.size()) {
      this.pc = this.instruments.get(this.pcIndex);
      return true;
    }
    return false;
  }

  public IR getPc() {
    return this.pc;
  }

  public void push(final AviatorObject... args) {
    for (AviatorObject arg : args) {
      this.operands.push(arg);
    }
  }

  public AviatorObject peek() {
    return this.operands.peek();
  }

  public AviatorObject pop() {
    return this.operands.pop();
  }
}
