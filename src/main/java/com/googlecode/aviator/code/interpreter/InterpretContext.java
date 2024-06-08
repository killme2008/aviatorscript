package com.googlecode.aviator.code.interpreter;

import java.util.ArrayDeque;
import java.util.List;
import com.googlecode.aviator.InterpretExpression;
import com.googlecode.aviator.lexer.token.Token;
import com.googlecode.aviator.parser.VariableMeta;
import com.googlecode.aviator.runtime.RuntimeUtils;
import com.googlecode.aviator.runtime.type.AviatorJavaType;
import com.googlecode.aviator.runtime.type.AviatorNil;
import com.googlecode.aviator.runtime.type.AviatorObject;
import com.googlecode.aviator.utils.Env;

/**
 * Eval IR context.
 *
 * @author dennis(killme2008@gmail.com)
 *
 */
public class InterpretContext {
  private final ArrayDeque<AviatorObject> operands = new ArrayDeque<>();
  private IR pc;
  private int pcIndex = -1;
  private IR[] instruments = new IR[0];
  private final Env env;
  private final InterpretExpression expression;
  private boolean reachEnd;
  private final boolean trace;


  public InterpretContext(final InterpretExpression exp, final List<IR> instruments,
      final Env env) {
    this.expression = exp;
    this.instruments = instruments.toArray(this.instruments);
    this.env = env;
    this.trace = RuntimeUtils.isTracedEval(env);
    next();
  }

  public AviatorJavaType loadVar(final VariableMeta v) {
    return this.expression.loadVar(v);
  }

  public AviatorObject loadConstant(final Token<?> token) {
    return this.expression.loadConstant(token);
  }

  public boolean isReachEnd() {
    return this.reachEnd;
  }

  public ArrayDeque<AviatorObject> getOperands() {
    return this.operands;
  }

  public void clearStack() {
    this.operands.clear();
  }

  public void jumpTo(final int tpc) {
    if (tpc == this.instruments.length) {
      this.pc = null;
      this.pcIndex = -1;
      this.reachEnd = true;
      return;
    }
    this.pcIndex = tpc;
    this.pc = this.instruments[this.pcIndex];
  }

  public InterpretExpression getExpression() {
    return this.expression;
  }

  public Env getEnv() {
    return this.env;
  }

  public boolean next() {
    if (this.reachEnd) {
      return false;
    }
    this.pcIndex++;
    if (this.pcIndex < this.instruments.length) {
      this.pc = this.instruments[this.pcIndex];
      return true;
    }
    return false;
  }

  public IR getPc() {
    return this.pc;
  }

  public void push(AviatorObject arg) {
    if (arg == null) {
      arg = AviatorNil.NIL;
    }
    this.operands.push(arg);
  }

  public AviatorObject peek() {
    return this.operands.peek();
  }

  public AviatorObject pop() {
    return this.operands.pop();
  }

  public String descOperandsStack() {
    StringBuilder sb = new StringBuilder("<Stack, [");
    int i = this.operands.size();
    for (AviatorObject obj : this.operands) {
      sb.append(obj.desc(this.env));
      if (--i > 0) {
        sb.append(", ");
      }
    }
    sb.append("]>");
    return sb.toString();
  }

  /**
   * Move pc to next and execute it.
   */
  public void dispatch() {
    this.dispatch(true);
  }

  /**
   * dispatch next IR execution.
   *
   * @param whether to move pc next.
   */
  public void dispatch(final boolean next) {
    if (next && !next()) {
      return;
    }

    if (this.pc != null) {
      if (this.pc.mayBeCost()) {
        RuntimeUtils.checkExecutionTimedOut(env);
      }
      if (this.trace) {
        RuntimeUtils.printlnTrace(this.env, "    " + this.pc + "    " + descOperandsStack());
      }
      this.pc.eval(this);
    }
  }
}
